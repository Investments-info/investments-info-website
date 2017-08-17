{-# LANGUAGE OverloadedStrings #-}

import Web.Scotty
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
main = scotty 3000 $ do
  get "/" $ do
    html $ mconcat ["<h1>Yahoo historical data downloader</h1>"]
