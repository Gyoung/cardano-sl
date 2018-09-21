{-# LANGUAGE LambdaCase #-}

-- | Module for small utility functions.
module Cardano.Wallet.Util
    ( -- * Miscellaneous
      (|>)
    , getsModify
    , unsafeElemIndex
    , runAsync

    -- * String manipulation
    , headToLower
    , stripFieldPrefix
    , mkJsonKey
    , kToS
    , sToK

    -- * Unsafe Type-Coercion from String
    , unsafeIPFromString
    , unsafeBoolFromString
    , unsafeNetworkAddressFromString
    , unsafeSeverityFromString

    -- * Time
    , defaultApiTimeLocale
    , apiTimeFormat
    , parseApiUtcTime
    , showApiUtcTime

    -- * CLI <--> Arg
    , ArgType
    , varFromParser
    , execParserEnv

    -- * NetworkAddress manipulation
    , nextNtwrkAddr
    , nextStringAddr
    , ntwrkAddrToString
    , ntwrkAddrToBaseUrl
    , ntwrkAddrToNodeAddr
    ) where

import qualified Prelude
import           Universum hiding (takeWhile)

import           Control.Concurrent.Async (Async, async, race, wait)
import           Control.Lens (at)
import qualified Data.Aeson as Aeson
import           Data.Attoparsec.ByteString.Char8 (IResult (..), parse,
                     skipWhile, string, takeWhile)
import qualified Data.Attoparsec.Internal.Types as Atto.Internal
import qualified Data.ByteString.Char8 as B8
import           Data.Char (isUpper, toLower)
import qualified Data.Char as Char
import           Data.IP (IP)
import           Data.List (elemIndex)
import           Data.Map (Map)
import           Data.Maybe (fromJust)
import qualified Data.Time as Time
import           Formatting (bprint, build, formatToString, (%))
import qualified Formatting.Buildable
import           Options.Applicative (ParseError (ShowHelpText), Parser,
                     ParserHelp (..), ParserInfo, ParserResult, execFailure,
                     execParserPure, info, parserFailure)
import qualified Options.Applicative as Parser
import           Options.Applicative.Help.Chunk (Chunk (..))
import qualified Safe
import           Servant.Client (BaseUrl (..), Scheme (..))
import qualified Text.Parsec as Parsec

import           Pos.Core.NetworkAddress (NetworkAddress, addrParser)
import           Pos.Infra.Network.DnsDomains (NodeAddr (..))
import           Pos.Util.Log.Severity (Severity (..))


-- * Miscellaneous

-- | Provide a default value for an Alternative
(|>) :: Alternative f => f a -> a -> f a
f |> a = f <|> pure a
infix 4 |>


-- | Get index of an element from a list, throw meaningful error upon failure
unsafeElemIndex
    :: (Eq a, Show a)
    => a
    -> [a]
    -> Int
unsafeElemIndex x xs =
    maybe
        (error $ "expected '" <> show x <> "' to be an element of " <> show xs)
        identity
        (x `elemIndex` xs)


-- | Apply a function using the current state that returns a value and modifies
-- the state.
getsModify :: (s -> (a, s)) -> State s a
getsModify f = do
    (a, s') <- gets f
    put s' >> return a


-- | Run an 'Async' non-terminating action giving some result to a caller
-- continuation.
--
-- Example:
--
--     (handle, res) <- runAsync $ \yield -> do
--        print "This happens in an asynchronous thread"
--        yield 14
--        threadDelay 10000000
--        return 42
--
--      print res             -- 14, immediately
--      wait handle >>= print -- 42, after 10s
runAsync :: ((b -> IO ()) -> IO a) -> IO (Async a, b)
runAsync action = do
    -- In case an error is thrown in action', the 'takeMVar' will block
    -- indefinitely. As a result, caller will fail with a very non-helpful
    -- message if we were simply going for `takeMVar mvar`:
    --
    --     "thread blocked indefinitely in an MVar operation"
    --
    -- With this little trick, we make sure not to wait indefinitely on the
    -- MVar. Beside, we shouldn't _ever_ reach the `Left` case below because
    -- 'wait' will re-throw any exception that occurs in the async action.
    -- So, the only way to reach the `Left` case here is to have `handle`
    -- terminating "normally" which can't happen.
    mvar   <- newEmptyMVar
    handle <- async (action (putMVar mvar))
    result <- race (wait handle) (takeMVar mvar)
    case result of
        Left _  -> fail "Action terminated unexpectedly"
        Right b -> return (handle, b)


-- * String manipulation utils

headToLower :: String -> Maybe String
headToLower []     = Nothing
headToLower (x:xs) = Just $ toLower x : xs

stripFieldPrefix :: String -> String
stripFieldPrefix = dropWhile (not . isUpper)

mkJsonKey :: String -> String
mkJsonKey s = fromMaybe s . headToLower $ stripFieldPrefix s

-- | kebab-case to UPPER_SNAKE_CASE
kToS :: String -> String
kToS = map (Char.toUpper . replaceIf '-' '_')

-- | UPPER_SNAKE_CASE to kebab-case
sToK :: String -> String
sToK = map (Char.toLower . replaceIf '_' '-')

-- | Replace third argument by the second one if it matches the first one.
replaceIf :: Char -> Char -> Char -> Char
replaceIf want to x | x == want = to
replaceIf _ _ x     = x


-- * Unsafe Type-Coercion from string

-- | Parse a 'ByteString' into an 'IP'.
unsafeIPFromString :: String -> IP
unsafeIPFromString bytes =
    case Safe.readMay bytes of
        Nothing ->
            error $ "'unsafeIPFromBS' was given a malformed value that can't\
                \ be parsed to an IP: " <> toText bytes
        Just ip ->
            ip

unsafeBoolFromString :: String -> Bool
unsafeBoolFromString str =
    case Safe.readMay str of
        Nothing ->
            error $ "Tried to convert a 'String' to a 'Bool' but failed: " <> toText str
        Just b ->
            b

unsafeNetworkAddressFromString :: String -> NetworkAddress
unsafeNetworkAddressFromString str =
    case Parsec.parse addrParser "" (toText str) of
        Left err ->
            error $ "Tried to convert a 'String' to a 'NetworkAddress' but failed: "
                <> toText str <> ", " <> show err
        Right addr ->
            addr

unsafeSeverityFromString :: String -> Severity
unsafeSeverityFromString str =
    case Aeson.decodeStrict ("\"" <> B8.pack str <> "\"") of
        Nothing ->
            error $ "Tried to convert a 'String' to a 'Severity' but failed."

        Just severity ->
            severity


-- * Time

-- | Currently we support only American usage.
defaultApiTimeLocale :: Time.TimeLocale
defaultApiTimeLocale = Time.defaultTimeLocale

-- | Time format used in API. Corresponds to ISO-8601.
-- Example of time: "2018-03-07T16:20:27.477318"
--
-- Note: there is more neat support of time formats in time-1.9,
-- Data.Time.Format.ISO8601 module, but that version is barely applicable with
-- current LTS-9.1.
apiTimeFormat :: String
apiTimeFormat = Time.iso8601DateFormat (Just "%H:%M:%S%Q")

newtype UtcTimeParseError = UtcTimeParseError Text

instance Buildable UtcTimeParseError where
    build (UtcTimeParseError msg) = bprint ("UTC time parse error: "%build) msg

instance Show UtcTimeParseError where
    show = formatToString build

-- | Parse UTC time from API.
parseApiUtcTime :: Text -> Either UtcTimeParseError Time.UTCTime
parseApiUtcTime =
    first UtcTimeParseError .
    Time.parseTimeM False defaultApiTimeLocale apiTimeFormat .
    toString

-- | Encode UTC time for API.
showApiUtcTime :: Time.UTCTime -> Text
showApiUtcTime = toText . Time.formatTime defaultApiTimeLocale apiTimeFormat


-- * ENV var <--> CLI args

-- We want to comprehend any ENVironment variables as if they were CLI
-- arguments and flags, such that we can re-use code to parse them and build
-- **Args structure from them.
--
-- The tricky thing here is that ENVironment "flags" don't exist so to speak,
-- so we have to reflect on the opt-applicative@Parser to see whether we expect
-- a var to be an 'Arg' or 'Flag'. If we expect a 'Flag' then the underlying
-- ENV var should be set to 'True' or 'False'
--
-- This also makes sure that we don't forget any CLI args and that's why, we
-- need to setup a full ENVironment before we actually start parsing. If one
-- variable is missing, it will throw a great deal.

data ArgType = Arg | Flag deriving Show

-- | Extract the list of ENV var from a 'Options.Applicative.Parser'
varFromParser
    :: Parser a             -- Target parser
    -> [(String, ArgType)]
varFromParser parser =
    foldParse [] (helpToByteString help)
  where
    -- Here is the little trick, we leverage the parserFailure which displays
    -- a usage with all possible arguments and flags and from this usage,
    -- we capture all arguments and flags as tuple (String, ArgType)
    (help, _, _) =
        let
            pInfo = info parser mempty
        in
            execFailure (parserFailure Parser.defaultPrefs pInfo ShowHelpText mempty) ""

    -- NOTE: 'fromJust' is safe here as there's always a usage
    helpToByteString :: ParserHelp -> ByteString
    helpToByteString =
        B8.pack . show . fromJust . unChunk . helpUsage

    -- | Convert a string argument to its corresponding ENV var
    argToVar :: String -> (String, ArgType)
    argToVar arg = case elemIndex ' ' arg of
        Nothing -> (kToS (drop 2 arg), Flag)
        Just i  -> (kToS (drop 2 (take i arg)), Arg)

    foldParse :: [(String, ArgType)] -> ByteString -> [(String, ArgType)]
    foldParse xs str = case parse (argToVar . B8.unpack <$> capture) str of
        Fail{}      -> xs
        Partial{}   -> xs
        Done rest x -> foldParse (x : xs) rest

    capture :: Atto.Internal.Parser ByteString ByteString
    capture =
        skipWhile (/= '[') *> string "[" *> takeWhile (/= ']') <* string "]"


-- | Run a parser from environment variables rather than command-line arguments
execParserEnv
    :: Map String String   -- ^ The full environment ENV
    -> [(String, ArgType)] -- ^ The restricted variables to consider within the ENV
    -> ParserInfo a        -- ^ A corresponding CLI
    -> ParserResult a
execParserEnv env vars pInfo = do
    let args = mapMaybe (lookupEnv >=> varToArg) vars
    execParserPure Parser.defaultPrefs pInfo args
  where
    -- | Lookup a value at a given 'Key' in the environment and add it to
    -- the tuple ('Key', 'ArgType')
    lookupEnv :: (String, ArgType) -> Maybe (String, ArgType, String)
    lookupEnv (k, t) =
        (k, t, ) <$> (env ^. at k)

    -- | Convert an environment variable to its argument, with value. Returns
    -- 'Nothing' when Flags are given and turned off. 'Just arg' otherwise.
    varToArg :: (String, ArgType, String) -> Maybe String
    varToArg = \case
        (k, Flag, "True") -> Just ("--" <> sToK k)
        (_, Flag, _)      -> Nothing
        (k, Arg, v)       -> Just ("--" <> sToK k <> "=" <> v)


-- * 'NetworkAddress' manipulations

-- | Get the next 'NetworkAddress' given an index
nextNtwrkAddr :: Word16 -> NetworkAddress -> NetworkAddress
nextNtwrkAddr i (host, port) =
    (host, port + i)


-- | Get the next 'NextworkAddress' from a string
nextStringAddr :: Word16 -> String -> String
nextStringAddr i =
    ntwrkAddrToString . nextNtwrkAddr i . unsafeNetworkAddressFromString


-- | Convert a 'NetworkAddress' to an ENV var
ntwrkAddrToString :: NetworkAddress -> String
ntwrkAddrToString (host, port) =
    B8.unpack host <> ":" <> show port


ntwrkAddrToBaseUrl :: NetworkAddress -> BaseUrl
ntwrkAddrToBaseUrl (host, port) =
    BaseUrl Https (B8.unpack host) (fromIntegral port) mempty


ntwrkAddrToNodeAddr :: NetworkAddress -> NodeAddr a
ntwrkAddrToNodeAddr (addr, port) =
    NodeAddrExact (unsafeIPFromString $ B8.unpack addr) (Just port)
