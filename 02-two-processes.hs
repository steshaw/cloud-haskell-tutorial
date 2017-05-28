#!/usr/bin/env stack
{-
  stack script --resolver lts-8.5
    --package distributed-process
    --package network-transport-tcp
    --
    -Wall -fwarn-tabs
-}

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Distributed.Process
import Control.Distributed.Process.Node
import Data.Monoid ((<>))
import Network.Transport.TCP (createTransport, defaultTCPParameters)

echoProcess :: Process ()
echoProcess = forever $
  -- Test our matches in order against each message in the queue.
  receiveWait [match logMessage, match replyBack]
  where
    logMessage :: String -> Process ()
    logMessage msg = say $ "handling " <> msg

    replyBack :: (ProcessId, String) -> Process ()
    replyBack (sender, msg) = send sender msg

withNode :: LocalNode -> IO ()
withNode node =
  runProcess node $ do
    -- Spawn another worker on the local node.
    echoPid <- spawnLocal $ forever echoProcess

    -- The `say` function sends a message to a process registered as "logger".
    -- By default, this process simply loops through its mailbo and sends
    -- and received log message string it finds to stderr.

    say "send some messages!"
    send echoPid "hello"
    self <- getSelfPid
    send echoPid (self, "hello")

    -- `expectTimeout` waits for a message or times out after a "delay".
    m <- expectTimeout 1000000
    case m of
      Nothing -> die "nothing came back!"
      Just s -> say $ "got " <> s ++ " back!"

    -- Without the following delay, the process sometimes exits before the
    -- messages are exchanged.
    liftIO $ threadDelay 2000000

main :: IO ()
main = do
  result <- createTransport "localhost" "10501" defaultTCPParameters
  case result of
    Left err -> print err
    Right t -> do
      node <- newLocalNode t initRemoteTable
      withNode node
