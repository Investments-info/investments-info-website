{-# LANGUAGE OverloadedStrings #-}

module Scripts.YahooHistorical where

-- import Import

import           Data.Aeson            (Value)
import qualified Data.ByteString.Char8 as S8
import qualified Data.Yaml             as Yaml
import           Network.HTTP.Simple

api_url :: Request
api_url = "https://query.yahooapis.com/v1/public/yql?q=select%20*%20from%20yahoo.finance.quotes%20where%20symbol%20in%20%28%22SLV%22%29&env=store://datatables.org/alltableswithkeys"

getYahooHistorical :: IO ()
getYahooHistorical = do
    response <- httpJSON api_url

    putStrLn $ "The status code was: " ++
               show (getResponseStatusCode response)
    print $ getResponseHeader "Content-Type" response
    S8.putStrLn $ Yaml.encode (getResponseBody response :: Value)
