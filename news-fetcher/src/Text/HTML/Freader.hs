{-# OPTIONS_GHC -Wall -Werror #-}
{-# LANGUAGE OverloadedStrings #-}
module Text.HTML.Freader where

import Control.Exception.Safe (Exception, MonadThrow, SomeException (..), throwM)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Data.ByteString.Lazy as B
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

parseRss :: (MonadThrow m, Typeable a, MonadIO m) => B.ByteString -> m a
parseRss bs = res
    where
      res =
        case parseLBS def bs of
          Left (SomeException a) -> return $ throwM $ RssException (show a) (typeRep res)
          Right d                -> return _
