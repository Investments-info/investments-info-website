{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.HTML.Freader where

import Control.Exception.Safe (Exception, MonadThrow, SomeException (..), throwM)
import Control.Monad (join)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy as B
import Data.Text hiding (concat, pack)
import Data.Typeable (TypeRep, Typeable, typeRep)
import Text.XML

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

parseFeed ::  Document -> Document -- m (n a)
parseFeed d = d

parseRss :: (MonadIO m, MonadThrow n, Typeable a) => IO B.ByteString -> m (n a)
parseRss bs = res
  where
    res =
      case parseLBS def <$> bs of
        Left (SomeException a) -> throwM $ RssException (decodeUtf8 a) (typeRep res)
        Right d                -> return $ parseFeed d

