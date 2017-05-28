#!/usr/bin/env stack
{- 
  stack script --resolver lts-8.5
    --package distributed-process
    --package network-transport-tcp
    --
    -Wall -fwarn-tabs
-}

{-# LANGUAGE TemplateHaskell #-}

import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Closure
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)

sampleTask :: (Int, String) -> Process ()
sampleTask (t, s) = liftIO (threadDelay (t * 1000000)) >> say s

remotable ['sampleTask]

myRemoteTable :: RemoteTable
myRemoteTable = Main.__remoteTable initRemoteTable

main :: IO ()
main = do
  Right transport <- createTransport "127.0.0.1" "10501" defaultTCPParameters
  node <- newLocalNode transport myRemoteTable
  runProcess node $ do
    us <- getSelfNode
    _ <- spawnLocal $ sampleTask (1, "using spawnLocal")
    _ <- spawn us $ $(mkClosure 'sampleTask) (2 :: Int, "using spawn")
    liftIO $ threadDelay (3 * 1000000)
