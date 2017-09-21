{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Helper.Helper where

import Database.Persist.Sql  (SqlBackend, rawSql, unSingle)
import Data.Text
import Data.Hashable
import Import
import LibYahoo (getYahooData)

data YahooData = YahooData
  { yahooDataDate :: UTCTime
  , yahooDataOpen :: Float
  , yahooDataHigh :: Float
  , yahooDataLow :: Float
  , yahooDataClose :: Float
  , yahooDataAdjClose :: Float
  , yahooDataVolume :: Int
  } deriving (Show, Eq)

data YD a where
    YahooD :: YD YahooData

companyCodes :: [String]
companyCodes = ["KO", "AAPL"]

truncateTables :: MonadIO m => ReaderT SqlBackend m [Text]
truncateTables = do
    result <- rawSql "DELETE FROM 'story';" []
    return (fmap unSingle result)

makeHash :: Hashable a => a -> Int
makeHash s = hash s

fetchYahooHistoricalData :: String -> HandlerT App IO ()
fetchYahooHistoricalData companyCode = do
  graphData <- liftIO (getYahooData companyCode)
  return ()
