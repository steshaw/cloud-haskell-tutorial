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

import Control.Concurrent (threadDelay)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable
import Control.Monad (forever)
import Data.Binary
import GHC.Generics
import Network.Transport.TCP (createTransport, defaultTCPParameters)

newtype Ping = Ping (SendPort Pong)
  deriving (Show, Generic)
instance Binary Ping

newtype Pong = Pong (SendPort Ping)
  deriving (Show, Generic)
instance Binary Pong

self :: Process ProcessId
self = getSelfPid

(!) :: Serializable a => ProcessId -> a -> Process ()
(!) = send

ping :: (SendPort Ping, ReceivePort Ping) -> Process ()
ping (pingSend, pingRecv) = forever $ do
  Ping pongSend <- receiveChan pingRecv
  liftIO $ print $ Ping pongSend
  sendChan pongSend (Pong pingSend)

pong :: (SendPort Pong, ReceivePort Pong) -> Process ()
pong (pongSend, pongRecv) = forever $ do
  Pong pingSend <- receiveChan pongRecv
  liftIO $ print $ Pong pingSend
  sendChan pingSend $ Ping pongSend

main :: IO ()
main = do
  result <- createTransport "localhost" "10501" defaultTCPParameters
  case result of
    Left err -> print err
    Right t -> do
      node <- newLocalNode t initRemoteTable
      runProcess node $ do
        pingChan <- newChan
        pongChan <- newChan
        liftIO $ print ("pingChan.send", fst pingChan)
        liftIO $ print ("pongChan.send", fst pongChan)
        _pingPid <- spawnLocal $ ping pingChan
        _pongPid <- spawnLocal $ pong pongChan
        liftIO $ putStrLn "Start ping<->pong"
        sendChan (fst pingChan) $ Ping (fst pongChan)
        liftIO $ threadDelay $ 1 * 1000 * 100
