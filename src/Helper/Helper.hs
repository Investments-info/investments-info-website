{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
module Helper.Helper where

import Database.Persist.Sql  (SqlBackend, rawSql, unSingle)
import Data.Text (Text)
import Data.Hashable
import Import
import LibYahoo (getYahooData)
import qualified Data.CSV.Conduit as C hiding (CSV)
import Data.CSV.Conduit.Conversion
import Control.Monad (mzero)
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy.UTF8 as DBLU
import qualified Data.Vector as V

-- data YahooData = YahooData
--   { yahooDataDate :: UTCTime
--   , yahooDataOpen :: Float
--   , yahooDataHigh :: Float
--   , yahooDataLow :: Float
--   , yahooDataClose :: Float
--   , yahooDataAdjClose :: Float
--   , yahooDataVolume :: Int
--   } deriving (Show, Eq)

-- data YD a where
--     YahooD :: YD YahooData

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



 ------------------------------------------------------------------------------------------------------

data YahooData = YahooData
  { yahooDataDate :: !Text
  , yahooDataOpen :: !Text
  , yahooDataHigh :: !Text
  , yahooDataLow :: !Text
  , yahooDataClose :: !Text
  , yahooDataAdjClose :: !Text
  , yahooDataVolume :: !Text
  } deriving (Show, Eq)

instance FromRecord YahooData where
  parseRecord v
    | length v == 7 =
      YahooData <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*>
      v .! 5 <*>
      v .! 6
    | otherwise = mzero

instance ToRecord YahooData where
  toRecord (YahooData yahooDataDate yahooDataOpen yahooDataHigh yahooDataLow yahooDataClose yahooDataAdjClose yahooDataVolume) =
    record
      [ toField yahooDataDate
      , toField yahooDataOpen
      , toField yahooDataHigh
      , toField yahooDataLow
      , toField yahooDataClose
      , toField yahooDataAdjClose
      , toField yahooDataVolume
      ]

readToType = do
  yd <- getYahooData "AAPL"
  let dataList = lines $ DBLU.toString yd
  let vector = fromList dataList
  let yList = fmap pack vector
  return undefined -- $ fmap parseRecord yList
