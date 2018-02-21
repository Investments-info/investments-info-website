{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Text.HTML.Freader where

import           Control.Exception.Safe (Exception, SomeException (..), try)
import           Control.Lens (set)
import           Control.Monad.Except (ExceptT, lift, runExceptT, throwError)
import           Data.ByteString.Lazy as B hiding (map, zip)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Text.XML (Document, def, parseLBS)
import           Text.XML.Cursor

rssLink :: T.Text
rssLink = "http://feeds.reuters.com/reuters/businessNews"

type RssM = ExceptT String IO

data RssFeed = RssFeed
  { rssTitle :: T.Text
  , rssUrl   :: T.Text
  } deriving Show

data RssException = RssException String deriving Show


instance Exception RssException

parseRss :: B.ByteString -> RssM Document
parseRss bs = res
    where
      res =
        case parseLBS def bs of
          Left (SomeException a) -> throwError (show a)
          Right d                -> return d

getFeed :: T.Text -> RssM Document
getFeed rssUrl = do
  manager <- lift $ newManager $ managerSetProxy noProxy tlsManagerSettings
  lift $ setGlobalManager manager
  rssRequest <- parseRequest $ T.unpack rssUrl
  crumb <- lift $
    try (httpLbs rssRequest manager) :: RssM (Either RssException (Response ByteString))
  case fmap responseBody crumb of
    Left (RssException e) -> throwError e
    Right a               -> parseRss a

-- parseDocument :: IO [RssFeed]
parseDocument = do
    doc <- runExceptT (getFeed rssLink)
    -- _ <- print doc
    case doc of
      Left e -> return $ [RssFeed {rssTitle = "", rssUrl = ""}]
      Right d -> do
        let cursor = fromDocument d
        let titles = child cursor >>= element "channel" >>= child >>= element "item" >>= child >>= element "title" >>= descendant >>= content
        let links = child cursor >>= element "channel" >>= child >>= element "item" >>= child >>= element "link" >>= descendant >>= content
        -- let description = child cursor >>= element "channel" >>= child >>= element "item" >>= child >>= element "description" >>= descendant >>= content
        -- _ <- print $ map ((set rssDescription) . (set rssUrl) . createFeedTitle) titles links description
        let joinData = zip titles links
        let f = map createFeed joinData
        undefined -- $


createFeed :: (T.Text, T.Text) -> RssFeed
createFeed l = RssFeed { rssTitle = fst l, rssUrl = snd l}
