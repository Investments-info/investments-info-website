{-# LANGUAGE OverloadedStrings #-}

import Network.Wreq

import Control.Lens
import Data.Aeson (toJSON)

-- Easy traversal of JSON data.
import Data.Aeson.Lens (key, nth)
import Data.Text



-- | api url for data fetching
api_url :: Text
api_url = "https://query1.finance.yahoo.com/v7/finance/download/%s?period1=%s&period2=%s&interval=%s&events=history&crumb=%s"

-- | url for the api login
login_url :: Text
login_url = "https://finance.yahoo.com/quote/"

-- | Build the url for the api call
buildApiUrl :: [Char] -> [Char] -> [Char] -> [Char] -> [Char] -> [Char]
buildApiUrl download period1 period2 interval crumb =
    "https://query1.finance.yahoo.com/v7/finance/download/" ++ download ++
    "?period1=" ++ period1 ++ "&period2=" ++ period2 ++ "&interval=" ++
     interval ++ "&events=history&crumb=" ++ crumb

main :: IO ()
main = do
    putStrLn "Yahoo historical data downloader"
    putStrLn "get http://httpbin.org/get"
    r <- get "http://httpbin.org/get"
    putStrLn "response status : "
    print (r ^. responseStatus . statusCode)
