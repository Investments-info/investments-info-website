module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import Control.Monad.Catch
import Control.Monad.Except
import Control.Distributed.Process
import Data.Default
import Control.Distributed.Process.Node
import Network.Transport.TCP (createTransport, defaultTCPParameters)

data ServiceConfig =
  ServiceConfig { serviceConfigHost  :: Text
                , serviceConfigPort  :: Text
                , serviceConfigPeers :: [PeerConfig]
                } deriving (Show)

instance Default ServiceConfig where
  def = ServiceConfig "127.0.0.1" "5300" []

instance FromJSON ServiceConfig where
  parseJSON = withObject "service configuration" $ \obj ->
    ServiceConfig <$> obj .: "host"
                  <*> obj .: "port"
                  <*> obj .: "peers"

replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender msg

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg

withTransport :: (MonadMask m, MonadIO m) =>
                 ServiceConfig -> (Transport -> m a) -> m a
withTransport ServiceConfig{..} action = do
  etransport <- liftIO (createTransport (unpack serviceConfigHost)
                                        (unpack serviceConfigPort)
                                        ((,) (unpack serviceConfigHost))
                                        defaultTCPParameters)
  case etransport of
    Left  err -> fail (show err)
    Right tra -> action tra `finally` liftIO (closeTransport tra)



main :: IO ()
main = do
  Right t <- createTransport
    (unpack serviceConfigHost)
    (unpack serviceConfigPort)
    ((,) (unpack serviceConfigHost)) defaultTCPParameters
  "127.0.0.1" "10501"  defaultTCPParameters
  node <- newLocalNode t initRemoteTable
  runProcess node $ do
    -- Spawn another worker on the local node
    echoPid <- spawnLocal $ forever $ do
      -- Test our matches in order against each message in the queue
      receiveWait [match logMessage, match replyBack]

    -- The `say` function sends a message to a process registered as "logger".
    -- By default, this process simply loops through its mailbox and sends
    -- any received log message strings it finds to stderr.

    say "send some messages!"
    send echoPid "hello"
    self <- getSelfPid
    send echoPid (self, "hello")

    -- `expectTimeout` waits for a message or times out after "delay"
    m <- expectTimeout 1000000
    case m of
      -- Die immediately - throws a ProcessExitException with the given reason.
      Nothing  -> die "nothing came back!"
      Just s -> say $ "got " ++ s ++ " back!"

    -- Without the following delay, the process sometimes exits before the messages are exchanged.
    liftIO $ threadDelay 2000000

