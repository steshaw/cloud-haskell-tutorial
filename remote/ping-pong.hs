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

import Control.Concurrent
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable
import Data.Binary
import GHC.Generics
import Network.Transport.TCP (createTransport, defaultTCPParameters)

newtype Ping = Ping ProcessId
  deriving (Show, Generic)
instance Binary Ping

newtype Pong = Pong ProcessId
  deriving (Show, Generic)
instance Binary Pong

self :: Process ProcessId
self = getSelfPid

(!) :: Serializable a => ProcessId -> a -> Process ()
(!) = send

ping :: Process ()
ping = do
  Ping pid <- expect
  liftIO $ print $ Ping pid
  self' <- self
  pid ! Pong self'
  ping

pong :: Process ()
pong = do
  Pong pid <- expect
  liftIO $ print $ Pong pid
  self' <- self
  pid ! Ping self'
  pong

main :: IO ()
main = do
  result <- createTransport "localhost" "10501" defaultTCPParameters
  case result of
    Left err -> print err
    Right t -> do
      node <- newLocalNode t initRemoteTable
      runProcess node $ do
        pingPid <- spawnLocal ping
        pongPid <- spawnLocal pong
        liftIO $ print ("pingPid", pingPid)
        liftIO $ print ("pongPid", pongPid)
        self' <- self
        pingPid ! Ping self'
        Pong pid <- expect
        liftIO $ print ("got pong from ", pid)
        liftIO $ putStrLn "Start ping<->pong"
        pingPid ! Ping pongPid
      liftIO $ threadDelay $ 1 * 1000 * 1000
