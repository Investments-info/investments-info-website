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

intervals :: [Text]
intervals = ["1d", "1wk", "1mo"]

-- this url is used to get the cookie
cookie_url :: Text -> Text
cookie_url ticker =  "https://finance.yahoo.com/quote/" <> ticker <> "/history"

api_url :: Text -> Text -> Text -> Text -> Text -> Maybe Request
api_url ticker startDate endDate interval crumb = parseRequest $ unpack $ "https://query1.finance.yahoo.com/v7/finance/download/"
    <> ticker <>
    "?period1="
    <> startDate <>
    "&period2="
    <> endDate <>
    "&interval="
    <> interval <>
    "&events=history&crumb="
    <> crumb

getYahooHistorical :: IO ()
getYahooHistorical = do
  request <- parseRequest (unpack $ cookie_url "AAPL")
  response <- httpLBS $ request
  let responseHeaders = getResponseHeaders response :: [(HeaderName, ByteString)]
  let cookieDict = lookup "set-cookie" responseHeaders
  case cookieDict of
      Nothing -> print ("No set-cookie key found" :: Text)
      Just a -> do
          let cookiesList =  parseCookies a
              ycookie = lookup "B" cookiesList
          case ycookie of
              Nothing -> print ("No B key found" :: Text)
              Just c -> print c
