{-| Demo cluster of wallet nodes. See demo/README.md -}

{-# LANGUAGE LambdaCase #-}

module Cardano.Wallet.Demo
    (
    -- * Types
      NodeName (..)
    , NodeType (..)
    , RunningNode (..)

    -- * Start Cluster
    , startCluster
    , startNode

    -- * Monitor cluster
    , MaxWaitingTime (..)
    , waitForNode

    -- * Configurations
    , demoTopology
    , demoTLSConfiguration
    , prepareEnvironment
    ) where

import qualified Prelude
import           Universum hiding (keys, (%~), (.~), _2)

import           Control.Concurrent (threadDelay)
import           Control.Concurrent.Async (Async, race)
import           Control.Lens (Field2 (..), at, (%~), (.~), (?~))
import           Data.Aeson (object, (.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as BL
import           Data.List (stripPrefix, (\\))
import           Data.Map (Map, (!))
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import           Options.Applicative (handleParseResult, info)
import           System.Directory (createDirectoryIfMissing)
import           System.Environment (getEnvironment)
import           System.FilePath.Posix (takeDirectory, (</>))
import           System.IO.Temp (withSystemTempDirectory)

import           Cardano.Wallet.Action (actionWithWallet)
import           Cardano.Wallet.API.V1.Types (ForceNtpCheck (..))
import           Cardano.Wallet.Client (ClientError (..), ServantError (..),
                     WalletClient (getNodeInfo))
import           Cardano.Wallet.Client.Http (credentialLoadX509, mkHttpClient,
                     mkHttpsManagerSettings, newManager, readSignedObject)
import           Cardano.Wallet.Server.CLI (NewWalletBackendParams (..),
                     walletBackendParamsParser)
import           Cardano.Wallet.Util (execParserEnv, getsModify, nextNtwrkAddr,
                     nextStringAddr, ntwrkAddrToBaseUrl, ntwrkAddrToNodeAddr,
                     runAsync, unsafeBoolFromString, unsafeElemIndex,
                     unsafeNetworkAddressFromString, unsafeSeverityFromString,
                     varFromParser, (|>))
import           Cardano.X509.Configuration (CertConfiguration (..),
                     CertDescription (..), DirConfiguration (..),
                     ServerConfiguration (..), TLSConfiguration (..),
                     fromConfiguration, genCertificate)
import           Data.X509.Extra (genRSA256KeyPair, writeCertificate,
                     writeCredentials)
import           Network.Broadcast.OutboundQueue (MaxBucketSize (..))
import           Pos.Client.CLI.NodeOptions (commonNodeArgsParser,
                     nodeArgsParser)
import           Pos.Client.CLI.Params (loggingParams)
import           Pos.Core.NetworkAddress (NetworkAddress)
import           Pos.Infra.Network.DnsDomains (DnsDomains (..))
import           Pos.Infra.Network.Types (NodeName (..), NodeType (..))
import           Pos.Infra.Network.Yaml (AllStaticallyKnownPeers (..),
                     NodeMetadata (..), NodeRegion (..), NodeRoutes (..),
                     Topology (..))
import           Pos.Launcher (LoggingParams (..), actionWithCoreNode,
                     launchNode)
import           Pos.Util.CompileInfo (withCompileInfo)
import           Pos.Util.Log.LoggerConfig (LoggerConfig)
import           Pos.Web.Types (TlsClientParams (..), TlsParams (..))


-- | A type representing a running node. The process is captured within the
-- 'Async' handle. For wallet nodes, there's an exta 'WalletClient' configured
-- to talk to the underlying node API.
data RunningNode m
    = RunningCoreNode   NodeName (Async ())
    | RunningRelayNode  NodeName (Async ())
    | RunningWalletNode NodeName (WalletClient m) (Async ())


-- | Start a cluster of wallet nodes in different threads.
-- Nodes get their (optional) arguments from the ENV.
--
-- For more details, look at demo/README.md
startCluster
    :: String                 -- ^ A prefix. Only ENV vars with this prefix will be considered
    -> [(NodeName, NodeType)] -- ^ A list of node names forming the cluster
    -> IO [RunningNode IO]
startCluster prefix nodes = do
    env <- toPrefixedEnv <$> getEnvironment
    forM nodes $ \node@(nodeId, nodeType) -> do
        (handle, running) <- runAsync $ \yield -> withStateDirectory env $ \stateDir -> do
            let ((initTopology, initLoggerConfig, initTLS), nodeEnv) =
                    prepareEnvironment env node nodes stateDir

            case nodeType of
                NodeCore -> do
                    void (initTopology >> initLoggerConfig)
                    yield (RunningCoreNode nodeId)

                NodeRelay -> do
                    void (initTopology >> initLoggerConfig)
                    yield (RunningRelayNode nodeId)

                NodeEdge -> do
                    tls <- initTopology >> initLoggerConfig >> initTLS
                    client <- mkWalletClient nodeEnv tls
                    yield (RunningWalletNode nodeId client)

            startNode node nodeEnv
        return (running handle)
  where
    -- | Only consider ENV var that have the given prefix
    toPrefixedEnv :: [(String, String)] -> Env
    toPrefixedEnv =
        let
            withPrefix (k, v) = (,) <$> stripPrefix prefix k <*> pure v
        in
            Map.fromList . mapMaybe withPrefix

    -- | Create a WalletClient instance from a given Env and TLS Params
    mkWalletClient :: Env -> TlsClientParams -> IO (WalletClient IO)
    mkWalletClient env tls = do
        ca  <- readSignedObject (tpClientCaPath tls)
        ecr <- credentialLoadX509 (tpClientCertPath tls) (tpClientKeyPath tls)
        case ecr of
            Left err ->
                fail $ "Error decoding X509 certificates: " <> err

            Right cr -> do
                -- NOTE Safe for edge-nodes, the variable exists in the ENV
                let wAddr@(host, port) = unsafeNetworkAddressFromString $ env ! "WALLET_ADDRESS"
                let serverId = (B8.unpack host, B8.pack $ show port)

                manager <- newManager (mkHttpsManagerSettings serverId ca cr)
                return $ mkHttpClient (ntwrkAddrToBaseUrl wAddr) manager


-- | Maximum time, in second, a function should for something
newtype MaxWaitingTime = MaxWaitingTime Int deriving (Eq, Show)


-- | Make HttpRequest continuously for a while to wait after the node
waitForNode
    :: WalletClient IO -- ^ A Wallet Client configured against a given node
    -> MaxWaitingTime  -- ^ Maximum waiting time, in seconds
    -> IO ()
waitForNode wc (MaxWaitingTime s) = do
    res <- race (threadDelay $ s * oneSecond) waitForNode'
    case res of
        Left _ ->
            fail $ "Giving up waiting for node to start: it takes too long"

        Right _ ->
            return ()
  where
    oneSecond :: Int
    oneSecond = 1000000

    waitForNode' :: IO ()
    waitForNode' = do
        resp <- getNodeInfo wc NoNtpCheck
        case resp of
            Right _ ->
                return ()

            Left (ClientHttpError ConnectionError{}) ->
                threadDelay oneSecond >> waitForNode'

            Left err ->
                fail $ "Failed to wait for node to start: " <> show err


-- | Simple Type-Alias for readability. This mimics the actual ENV but in a pure
-- way. And bonus, we can use all sort of lenses and traversals on that!
type Env = Map String String


-- | Start a demo node (with wallet) using the given environment as a context.
-- This action never returns, unless the node crashes.
startNode
    :: (NodeName, NodeType) -- ^ The actual node name
    -> Env                  -- ^ A "simulation" of the system ENV as a 'Map String String'
    -> IO ()
startNode (NodeName nodeIdT, nodeType) env = do
    nArgs <- parseNodeArgs
    cArgs <- parseCommonNodeArgs
    let lArgs = getLoggingArgs cArgs
    case nodeType of
        NodeEdge -> do
            wArgs <- parseWalletArgs
            withCompileInfo $ launchNode nArgs cArgs lArgs (actionWithWallet wArgs)

        _ ->
            withCompileInfo $ launchNode nArgs cArgs lArgs actionWithCoreNode
  where
    parseNodeArgs = do
        let nVars = varFromParser nodeArgsParser
        let nInfo = info nodeArgsParser mempty
        handleParseResult $ execParserEnv env nVars nInfo

    parseCommonNodeArgs = do
        let cVars = varFromParser commonNodeArgsParser
        let cInfo = info commonNodeArgsParser mempty
        handleParseResult $ execParserEnv env cVars cInfo

    parseWalletArgs = do
        let wVars = varFromParser walletBackendParamsParser
        let wInfo = info walletBackendParamsParser mempty
        NewWalletBackendParams <$> handleParseResult (execParserEnv env wVars wInfo)

    -- NOTE
    -- Logging to the console is disabled. This is just noise when multiple
    -- nodes are running at the same time. Logs are available in the logfiles
    -- inside the state directory anyway. `tail -f` is a friend.
    getLoggingArgs cArgs = (loggingParams (fromString $ T.unpack nodeIdT) cArgs)
        { lpConsoleLog = Just False }


-- | Get Temporary Working / State Directory to work with.
--
-- use STATE_DIR as a directory if it's given,
-- otherwise, create a new system-level temp directory.
withStateDirectory
    :: Env                -- ^ A "simulation" of the system ENV
    -> (FilePath -> IO a) -- ^ Action to run with the state directory
    -> IO a
withStateDirectory env cb =
    case env ^. at "STATE_DIR" of
        Nothing ->
            withSystemTempDirectory "cardano-sl-wallet:demo" cb

        Just dir ->
            cb dir


-- | Setup the environment for the node. This is where we assign default values
-- to mandatory arguments and constructs the related configurations (topology,
-- logging, tls, ...).
--
-- It returns actions that can be ran at a higher level to create and get those
-- configurations as well as a modified ENV which has been hydrated with
-- everything needed by the node to start.
prepareEnvironment
    :: Env                      -- ^ ENVironment context with user-defined ENV vars
    -> (NodeName, NodeType)     -- ^ Related node identifier
    -> [(NodeName, NodeType)]   -- ^ All nodes, including the related one
    -> FilePath                 -- ^ Node State / Working directory
    -> ((IO Topology, IO LoggerConfig, IO TlsClientParams), Env)
prepareEnvironment baseEnv node@(NodeName nodeIdT, nodeType) nodes stateDir =
    flip runState baseEnv $ do
        modify withDefaultEnvironment
        topology <- getsModify prepareTopology
        logger   <- getsModify prepareLogger
        tls      <- getsModify prepareTLS
        return (topology, logger, tls)
  where
    nodeId :: String
    nodeId = T.unpack nodeIdT

    withDefaultEnvironment :: Env -> Env
    withDefaultEnvironment =
        case nodeType of
            NodeEdge -> withDefaultWalletEnvironment . withDefaultCoreEnvironment
            _        -> withDefaultCoreEnvironment

    withDefaultCoreEnvironment :: Env -> Env
    withDefaultCoreEnvironment env = env
        & at "CONFIGURATION_FILE" %~ (|> "../lib/configuration.yaml")
        & at "CONFIGURATION_KEY"  %~ (|> "default")
        & at "DB_PATH"            .~ (Just $ stateDir </> "db" </> nodeId)
        & at "LISTEN"             %~ (|> "127.0.0.1:3000")
        & at "LOG_SEVERITY"       %~ (|> "Debug")
        & at "NODE_ID"            .~ (Just nodeId)
        & at "REBUILD_DB"         %~ (|> "False")
        & at "SYSTEM_START"       %~ (|> "0")

    withDefaultWalletEnvironment :: Env -> Env
    withDefaultWalletEnvironment env = env
        & at "NO_CLIENT_AUTH"     %~ (|> "False")
        & at "WALLET_ADDRESS"     %~ (|> "127.0.0.1:8090")
        & at "WALLET_DB_PATH"     .~ (Just $ stateDir </> "wallet-db" </> nodeId)
        & at "WALLET_DOC_ADDRESS" %~ (|> "127.0.0.1:8190")
        & at "WALLET_REBUILD_DB"  %~ (|> "False")

    -- | Create the 'Topology' of the given node
    -- NOTE: The topology can't be overriden by ENV vars.
    prepareTopology :: Env -> (IO Topology, Env)
    prepareTopology env =
        let
            cIndex =
                fromIntegral $ unsafeElemIndex node nodes

            wIndex =
                fromIntegral $ unsafeElemIndex node (filter isEdgeNode nodes)

            addr =
                -- NOTE Safe when called after 'withDefaultEnvironment'
                unsafeNetworkAddressFromString (env ! "LISTEN")

            topologyPath =
                stateDir </> "topology" </> T.unpack nodeIdT <> ".json"

            (nodeNames, nodeTypes) =
                unzip nodes

            nodeAddrs =
                flip nextNtwrkAddr addr <$> (iterate (+1) 0)

            topology =
                demoTopology nodeType (zip3 nodeNames nodeTypes nodeAddrs)

            initTopology = do
                createDirectoryIfMissing True (takeDirectory topologyPath)
                BL.writeFile topologyPath (Aeson.encode topology)
                return topology
        in
            case nodeType of
                NodeEdge ->
                    ( initTopology
                    , env
                        & at "TOPOLOGY"           .~ Just topologyPath
                        & at "LISTEN"             .~ Nothing
                        & at "WALLET_ADDRESS"     %~ (fmap $ nextStringAddr wIndex)
                        & at "WALLET_DOC_ADDRESS" %~ (fmap $ nextStringAddr wIndex)
                    )

                _ ->
                    ( initTopology
                    , env
                        & at "TOPOLOGY" .~ Just topologyPath
                        & at "LISTEN"   %~ (fmap $ nextStringAddr cIndex)
                    )

    -- | Create a 'LoggerConfig' for the given node
    -- NOTE: The 'LoggerConfig' can't be overriden by ENV vars, however,
    -- the severity can be adjusted with an extra env var 'LOG_SEVERITY'
    prepareLogger :: Env -> (IO LoggerConfig, Env)
    prepareLogger env =
        let
            loggerConfigPath =
                stateDir </> "logs" </> nodeId <> ".json"

            logFilePath =
                stateDir </> "logs" </> nodeId <> ".log.pub"

            logSeverity =
                -- NOTE Safe when called after 'withDefaultEnvironment'
                unsafeSeverityFromString (env ! "LOG_SEVERITY")

            -- NOTE 1:
            -- Unfortunately, it appears that JSON instances of types from
            -- 'Pos.Util.Log.LoggerConfig' don't have the roundtrip property.
            -- Therefore, trying to parse a file generated from encoding a
            -- 'LoggerType' is hopeless.
            -- The representations don't match.
            loggerConfigJSON = object
                [ "rotation" .= object
                    [ "logLimit"  .= (104857600 :: Word64)
                    , "keepFiles" .= (1 :: Word)
                    ]
                , "loggerTree" .= object
                    [ "severity" .= logSeverity
                    , "files"    .= [ logFilePath ]
                    ]
                ]

            -- NOTE 'fromJust' is safe because we are making a valid JSON by hand.
            loggerConfig =
                fromJust $ Aeson.parseMaybe Aeson.parseJSON $ loggerConfigJSON

            initLoggerConfig = do
                createDirectoryIfMissing True (takeDirectory loggerConfigPath)
                BL.writeFile loggerConfigPath (Aeson.encode loggerConfigJSON)
                return loggerConfig
        in
            ( initLoggerConfig
            , env & at "LOG_CONFIG" .~ Just loggerConfigPath
            )

    -- | Create TLS Certificates configurations
    -- NOTE: The TLS configurations & certs can't be overriden by ENV vars.
    prepareTLS :: Env -> (IO TlsClientParams, Env)
    prepareTLS env =
        let
            noClientAuth =
                -- NOTE Safe when called after 'withDefaultEnvironment'
                unsafeBoolFromString (env ! "NO_CLIENT_AUTH")

            tlsBasePath =
                stateDir </> "tls" </> nodeId

            tlsParams = TlsParams
                { tpCertPath   = tlsBasePath </> "server.crt"
                , tpKeyPath    = tlsBasePath </> "server.key"
                , tpCaPath     = tlsBasePath </> "ca.crt"
                , tpClientAuth = not noClientAuth
                }

            tlsClientParams = TlsClientParams
                { tpClientCertPath = tlsBasePath </> "client.crt"
                , tpClientKeyPath  = tlsBasePath </> "client.key"
                , tpClientCaPath   = tlsBasePath </> "ca.crt"
                }

            (tlsConf, dirConf) =
                demoTLSConfiguration tlsBasePath

            initTLSEnvironment = do
                keys <- genRSA256KeyPair
                let (ca, cs) = fromConfiguration tlsConf dirConf genRSA256KeyPair keys
                (_, caCert) <- genCertificate ca
                forM_ cs $ \c -> do
                    createDirectoryIfMissing True (certOutDir c)
                    writeCredentials (certOutDir c </> certFilename c) =<< genCertificate c
                    writeCertificate (certOutDir c </> certFilename ca) caCert
                return tlsClientParams

            errTLSIrrelevant =
                fail "Attempted to initialize TLS environment for a non-edge node. \
                    \This is seemingly irrelevant: TLS is required for contacting \
                    \the Wallet API."
        in
            case nodeType of
                NodeEdge ->
                    ( initTLSEnvironment
                    , env
                        & at "TLSCERT" ?~ tpCertPath tlsParams
                        & at "TLSKEY"  ?~ tpKeyPath tlsParams
                        & at "TLSCA"   ?~ tpCaPath tlsParams
                    )

                _ ->
                    (errTLSIrrelevant, env)


-- | Demo TLS Configuration
demoTLSConfiguration
    :: FilePath -- ^ Directory to output TLS stuff
    -> (TLSConfiguration, DirConfiguration)
demoTLSConfiguration dir =
    ( TLSConfiguration
        { tlsCa = CertConfiguration
            { certOrganization = "IOHK - Demo"
            , certCommonName   = "Root Self-Signed CA"
            , certExpiryDays   = 365
            }
        , tlsServer = ServerConfiguration
            { serverAltNames      = "localhost" :| [ "127.0.0.1" ]
            , serverConfiguration = CertConfiguration
                { certOrganization = "IOHK - Demo"
                , certCommonName   = "Server Certificate"
                , certExpiryDays   = 365
                }
            }
        , tlsClients = pure CertConfiguration
            { certOrganization = "IOHK - Demo"
            , certCommonName   = "Client Certificate"
            , certExpiryDays   = 365
            }
        }
    , DirConfiguration
        { outDirServer  = dir
        , outDirClients = dir
        , outDirCA      = Just dir
        }
    )


-- | Create a default topology file structure for the given nodes associated
-- with their corresponding network addresses
demoTopology
    :: NodeType                               -- ^ Target node type (core, relay, edge ...)
    -> [(NodeName, NodeType, NetworkAddress)] -- ^ All fully qualified nodes
    -> Topology
demoTopology nodeType =
    case nodeType of
        NodeEdge ->
            TopologyBehindNAT 1 1 . mkRelays . filter isRelayNode
        _ ->
            TopologyStatic . mkStaticRoutes . filter (not . isEdgeNode)
  where
    mkRelays
        :: [(NodeName, NodeType, NetworkAddress)]
        -> DnsDomains a
    mkRelays =
        DnsDomains . pure . map (ntwrkAddrToNodeAddr . (^. _3))

    mkStaticRoutes
        :: [(NodeName, NodeType, NetworkAddress)]
        -> AllStaticallyKnownPeers
    mkStaticRoutes =
        AllStaticallyKnownPeers . Map.fromList . map mkStaticPeer . subPermutations

    mkStaticPeer
        :: ((NodeName, NodeType, NetworkAddress), [(NodeName, NodeType, NetworkAddress)])
        -> (NodeName, NodeMetadata)
    mkStaticPeer ((peerId, peerType, peerAddr), routes) =
        (peerId, mkStaticMetadata peerType peerAddr (mkRoutes routes))

    mkRoutes
        :: [(NodeName, NodeType, NetworkAddress)]
        -> NodeRoutes
    mkRoutes =
        NodeRoutes . map (pure . (^. _1))

    mkStaticMetadata
        :: NodeType
        -> NetworkAddress
        -> NodeRoutes
        -> NodeMetadata
    mkStaticMetadata nType (addr, port) routes = NodeMetadata
        { nmType       = nType
        , nmRegion     = NodeRegion "undefined"
        , nmRoutes     = routes
        , nmSubscribe  = DnsDomains []
        , nmValency    = 1
        , nmFallbacks  = 1
        , nmAddress    = ntwrkAddrToNodeAddr (addr, port)
        , nmKademlia   = False
        , nmPublicDNS  = False
        , nmMaxSubscrs = BucketSizeUnlimited
        }

    subPermutations :: Eq a => [a] -> [(a, [a])]
    subPermutations xs =
        map (\ys -> (h ys, ys)) (sizedSubsequences (length xs - 1) xs)
      where
        h ys = Prelude.head (xs \\ ys)

    sizedSubsequences :: Int -> [a] -> [[a]]
    sizedSubsequences n =
        filter ((== n) . length) . subsequences


--
-- (Internal) Helpers
--

isEdgeNode
    :: Field2 t t NodeType NodeType
    => t
    -> Bool
isEdgeNode =
    (== NodeEdge) . (^. _2)


isRelayNode
    :: Field2 t t NodeType NodeType
    => t
    -> Bool
isRelayNode =
    (== NodeRelay) . (^. _2)
