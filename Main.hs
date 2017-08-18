{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Network.Wreq

import Control.Lens
import Data.Aeson (ToJSON, FromJSON)

-- Easy traversal of JSON data.
import Data.Aeson.Lens (key, nth)
import Data.Text
import Data.Map as Map
import Data.Aeson (Value)
import Control.Monad.IO.Class
import GHC.Generics (Generic)
import qualified Control.Exception as E


type Resp = Response (Map String Value)

data Fdata = Fdata {
    fdataSymbol :: Maybe String
    ,fdataTimestampl :: Maybe String
    ,fdataTradingDay :: Maybe String
    ,fdataOpen :: Maybe Double
    ,fdataHigh :: Maybe Double
    ,fdataLow :: Maybe Double
    ,fdataClose :: Maybe Double
    ,fdataVolume :: Maybe Integer
    ,fdataOpenInterest :: Maybe String
   } deriving (Show, Generic)

instance ToJSON Fdata
instance FromJSON Fdata


-- | Barchart api
api_key = "b47d9c51c11fee9020293a37f9b65557"

history = "http://marketdata.websol.barchart.com/getHistory.json?apikey=" ++ api_key ++ "&symbol=IBM&type=daily&startDate=20160818000000"

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
  r <- get history
  -- This first conversion attempt will fail, but because we're using
  -- Either, it will not throw an exception that kills execution.
  let failing = asJSON r :: Either E.SomeException (Response [Int])
  print failing

  -- Our second conversion attempt will succeed.
  let succeeding = asJSON r :: Either E.SomeException (Response Fdata)
  print succeeding
