#!/usr/bin/env stack
{- 
  stack script --resolver lts-8.5
    --package distributed-process
    --package network-transport-tcp
    --
    -Wall -fwarn-tabs
-}

import Network.Transport.TCP (createTransport, defaultTCPParameters)
import Control.Distributed.Process
import Control.Distributed.Process.Node

main :: IO ()
main = do
  result <- createTransport "localhost" "10501" defaultTCPParameters
  case result of
    Left err -> print err
    Right t -> do
      node <- newLocalNode t initRemoteTable
      _ <- runProcess node $ do
        self <- getSelfPid
        send self "hello"
        hello <- expect :: Process String
        liftIO $ putStrLn hello
      pure ()
