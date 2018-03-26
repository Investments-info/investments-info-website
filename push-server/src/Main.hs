{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Control.Concurrent (threadDelay)
import           Control.Distributed.Process hiding (finally)
import           Control.Distributed.Process.Node
import           Control.Monad (forever)
import           Control.Monad.Catch
import           Control.Monad.Except
import           Data.Aeson
import           Data.Default
import           Data.Text (Text , unpack)
import           Network.Transport hiding (send)
import           Network.Transport.TCP
import           Network.Transport.TCP (createTransport, defaultTCPParameters)
import           System.Environment

data ServiceConfig = ServiceConfig
  { serviceConfigHost  :: Text
  , serviceConfigPort  :: Text
  } deriving (Show)

instance Default ServiceConfig where
  def = ServiceConfig "127.0.0.1" "10502"

instance FromJSON ServiceConfig where
  parseJSON =
    withObject "service configuration" $ \obj ->
      ServiceConfig <$> obj .: "host" <*> obj .: "port"

replyBack :: (ProcessId, String) -> Process ()
replyBack (sender, msg) = send sender msg

logMessage :: String -> Process ()
logMessage msg = say $ "handling " ++ msg

withTransport :: (MonadMask m, MonadIO m) =>
                 ServiceConfig -> (Transport -> m a) -> m a
withTransport ServiceConfig {..} action = do
  etransport <-
    liftIO
      (createTransport
         (unpack serviceConfigHost)
         (unpack serviceConfigPort)
         ((,) (unpack serviceConfigHost))
         defaultTCPParameters)
  case etransport of
    Left err  -> fail (show err)
    Right tra -> action tra `finally` liftIO (closeTransport tra)

main :: IO ()
main = do
  t <-
    createTransport
      (unpack (serviceConfigHost def))
      (unpack (serviceConfigPort def))
      ((,) (unpack (serviceConfigHost def)))
      defaultTCPParameters
  case t of
    Right t -> do
     node <- newLocalNode t initRemoteTable
     runProcess node $ do
       -- Spawn another worker on the local node
         echoPid <- do spawnLocal $ forever $ do
                         receiveWait [match logMessage, match replyBack]
         -- The `say` function sends a message to a process registered as "logger".
         -- By default, this process simply loops through its mailbox and sends
         -- any received log message strings it finds to stderr.
         say "send some messages!"
         send echoPid ("hello" :: Text)
         self <- getSelfPid
         send echoPid (self, ("hello" :: Text))
         -- `expectTimeout` waits for a message or times out after "delay"
         m <- expectTimeout 1000000
         case m of
           Nothing -> die ("nothing came back!" :: Text)
           Just s -> say $ "got " ++ s ++ " back!"
         liftIO $ threadDelay 2000000
    Left _ -> do
      print ("transport not created!" :: Text)
