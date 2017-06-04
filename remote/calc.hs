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

import Control.Distributed.Process
import Control.Distributed.Process.Node
import Control.Distributed.Process.Serializable
import Data.Binary
import GHC.Generics
import Network.Transport.TCP (createTransport, defaultTCPParameters)

self :: Process ProcessId
self = getSelfPid

(!) :: Serializable a => ProcessId -> a -> Process ()
(!) = send

data Add = Add ProcessId Double Double
  deriving (Show, Generic)
instance Binary Add
data Divide = Divide ProcessId Double Double
  deriving (Show, Generic)
instance Binary Divide
data DivByZero = DivByZero
  deriving (Show, Generic)
instance Binary DivByZero

calc :: Process ()
calc =
  receiveWait
    [ match $ \(Add pid num1 num2) -> pid ! (num1 + num2)
    , matchIf (\(Divide _ _ num2) -> num2 /= 0) $ 
               \(Divide pid num1 num2) -> pid ! (num1 / num2)
    , match $ \(Divide pid _ _) -> pid ! DivByZero
    ] >> calc

main :: IO ()
main = do
  result <- createTransport "localhost" "10501" defaultTCPParameters
  case result of
    Left err -> print err
    Right t -> do
      node <- newLocalNode t initRemoteTable
      runProcess node $ do
        calcPid <- spawnLocal calc
        liftIO $ print ("calcPid", calcPid)
        self' <- self

        calcPid ! Add self' 1 2
        (x :: Double) <- expect
        liftIO $ print ("1 + 2 = ", x)

        calcPid ! Divide self' 1 2
        (y :: Double) <- expect
        liftIO $ print ("1 / 2 = ", y)

        calcPid ! Divide self' 3 0
        (z :: DivByZero) <- expect
        liftIO $ print ("3 / 0 = ", z)
