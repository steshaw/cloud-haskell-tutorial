#!/usr/bin/env stack
{- 
  stack script --resolver lts-8.5
    --package distributed-process
    --package distributed-process-simplelocalnet
    --
    -Wall -fwarn-tabs
-}

import Control.Distributed.Process
import Control.Distributed.Process.Node (initRemoteTable)
import Control.Distributed.Process.Backend.SimpleLocalnet
import System.Environment (getArgs)

master :: Backend -> [NodeId] -> Process ()
master backend slaves = do
  -- Do something interesting with the slaves
  liftIO . putStrLn $ "Slaves: " ++ show slaves
  -- Terminate the slaves when the master terminates (this is optional)
  terminateAllSlaves backend

main :: IO ()
main = do
  args <- getArgs

  case args of
    ["master", host, port] -> do
      backend <- initializeBackend host port initRemoteTable
      startMaster backend (master backend)
    ["slave", host, port] -> do
      backend <- initializeBackend host port initRemoteTable
      startSlave backend
