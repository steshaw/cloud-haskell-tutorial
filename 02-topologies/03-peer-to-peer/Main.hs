#!/usr/bin/env stack
{-
  stack script --resolver lts-8.5
    --package distributed-process
    --package distributed-process-p2p
    --
    -Wall -fwarn-tabs
-}

import System.Environment (getArgs)
import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Backend.P2P as P2P
import Control.Monad (forever, mapM_)

helloProcess = do
  liftIO $ threadDelay 1000000 -- give dispatcher a second to discover other nodes
  nsendPeers "echo-server" "hello!"

main = do
  [host, port] <- getArgs

  P2P.bootstrap host port [P2P.makeNodeId "seedhost:9000"] initRemoteTable $
    helloProcess

{-
  backend <- initializeBackend host port initRemoteTable
  node    <- newLocalNode backend
  runProcess node $ forever $ do
    getPeers >>= mapM_ $ \peer -> nsend peer "echo-server" "hello!"
-}
