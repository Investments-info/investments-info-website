{-# LANGUAGE OverloadedStrings #-}
module Text.HTML.Freader where

import           Control.Exception.Safe (Exception, SomeException (..))
import           Data.ByteString.Lazy as B
import           Data.Typeable (TypeRep, Typeable, typeRep)
import           Text.XML (Document, def, parseLBS)

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
