{-# LANGUAGE OverloadedStrings #-}
module Text.HTML.Freader where

import           Control.Exception.Safe (Exception, SomeException (..), try)
import           Data.ByteString.Lazy as B
import qualified Data.Text as T
import           Data.Typeable (TypeRep, Typeable, typeRep)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Text.XML (Document, def, parseLBS)

rssLink :: T.Text
rssLink = "http://feeds.reuters.com/reuters/businessNews"

data RssException = RssException String TypeRep
    deriving (Typeable)

instance Show RssException where
  show (RssException s typ) = Prelude.concat
    [ "Unable to parse as "
    , show typ
    , ": "
    , show s
    ]

instance Exception RssException

parseRss :: B.ByteString -> Either RssException Document
parseRss bs = res
    where
      res =
        case parseLBS def bs of
          Left (SomeException a) -> Left $ RssException (show a) (typeRep res)
          Right d                -> Right d

getFeed :: T.Text -> IO (Either RssException Document)
getFeed rssUrl = do
  manager <- newManager $ managerSetProxy noProxy tlsManagerSettings
  setGlobalManager manager
  rssRequest <- parseRequest $ T.unpack rssUrl
  crumb <-
    try (httpLbs rssRequest manager) :: IO (Either RssException (Response ByteString))
  case fmap responseBody crumb of
    Left e  -> return $ Left $ RssException (show e) (typeRep (fmap responseBody crumb))
    Right a -> return $ parseRss a
