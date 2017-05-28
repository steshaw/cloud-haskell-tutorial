#!/usr/bin/env stack
{- 
  stack script --resolver lts-8.5
    --package distributed-process
    --package distributed-process-simplelocalnet
    --
    -Wall -fwarn-tabs
-}

import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable, runProcess)
import Control.Distributed.Process.Backend.SimpleLocalnet
import Control.Monad (forM_)
import System.Environment (getArgs)

main :: IO ()
main = do
  [host, port] <- getArgs

  backend <- initializeBackend host port initRemoteTable
  node <- newLocalNode backend
  peers <- findPeers backend 1000000
  runProcess node $
    forM_ peers $ \peer ->
      nsendRemote peer "echo-server" "hello!"
