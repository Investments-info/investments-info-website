{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Scripts.YahooHistorical where

import Import hiding (print, lookup)
import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple
import           Web.Cookie
import Generics.Deriving


-- this url is used to get the cookie
cookie_url :: Text -> Text
cookie_url ticker =  "https://finance.yahoo.com/quote/" <> ticker <> "/history"

api_url :: Request
api_url = "https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.quotes%20where%20symbol%20in%20%28%22SLV%22%29&env=store://datatables.org/alltableswithkeys"

getYahooHistorical :: IO ()
getYahooHistorical = do
  request <- parseRequest (unpack $ cookie_url "AAPL")
  response <- httpLBS $ request
  let responseHeaders = getResponseHeaders response :: [(HeaderName, ByteString)]
  let cookieDict = lookup "set-cookie" responseHeaders
  case cookieDict of
      Nothing -> print "No set-cookie key found"
      Just a -> do
          let cookiesList =  parseCookies a
              ycookie = lookup "B" cookiesList
          case ycookie of
              Nothing -> print "No B key found"
              Just c -> print c
