#!/usr/bin/env stack
{- 
  stack script --resolver lts-8.5
    --package binary
    --package distributed-process
    --package network-transport-tcp
    --
    -Wall -fwarn-tabs
    -threaded -rtsopts -with-rtsopts=-N
-}

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable
import Data.Binary
import GHC.Generics

newtype Ping = Ping ProcessId
  deriving Generic
instance Binary Ping

newtype Pong = Pong ProcessId
  deriving Generic
instance Binary Pong

self :: Process ProcessId
self = getSelfPid

(!) :: Serializable a => ProcessId -> a -> Process ()
(!) = send

ping :: Process ()
ping = do
  Ping pid <- expect
  self' <- self
  pid ! (Pong self')
  ping

main :: IO ()
main = do
  result <- createTransport "localhost" "10501" defaultTCPParameters
  case result of
    Left err -> print err
    Right t -> do
      node <- newLocalNode t initRemoteTable
      runProcess node $ do
        pingPid <- spawnLocal ping
        liftIO $ print ("pingPid", pingPid)
        self' <- self
        pingPid ! (Ping self')
        Pong pid <- expect
        liftIO $ print ("got pong from ", pid)
