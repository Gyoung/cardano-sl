{-| Demo cluster of wallet nodes. See demo/README.md -}

{-# LANGUAGE LambdaCase #-}

module Main where

import           Universum

import           Control.Concurrent.Async (waitAny)
import           System.IO (BufferMode (..), hSetBuffering, stdout)

import           Cardano.Wallet.Demo (MaxWaitingTime (..), NodeName (..),
                     NodeType (..), RunningNode (..), startCluster,
                     waitForNode)


-- | Cluster configuration can be tweaked via ENV vars. Each ENV var is prefixed
-- with the following.
--
-- (e.g. `DEMO_NO_CLIENT_AUTH=True`)
prefix :: String
prefix = "DEMO_"


main :: IO ()
main = void $ do
    hSetBuffering stdout NoBuffering -- Instead of LineBuffering

    putTextLn "Cluster is starting..."
    cluster <- startCluster prefix
        [ ("core0", NodeCore)
        , ("core1", NodeCore)
        , ("core2", NodeCore)
        , ("core3", NodeCore)
        , ("relay", NodeRelay)
        , ("wallet", NodeEdge)
        ]
    handles <- forM cluster $ \case
        RunningCoreNode (NodeName nodeId) handle -> do
            putTextLn $ "..." <> nodeId <> " skipped!"
            return handle

        RunningRelayNode (NodeName nodeId) handle -> do
            putTextLn $ "..." <> nodeId <> " skipped!"
            return handle

        RunningWalletNode (NodeName nodeId) client handle -> do
            putText "..." >> waitForNode client (MaxWaitingTime 10)
            putTextLn $ nodeId <> " OK!"
            return handle
    putTextLn "Cluster is ready!"

    waitAny handles
