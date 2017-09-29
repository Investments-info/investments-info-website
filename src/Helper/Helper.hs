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
import qualified Data.CSV.Conduit as CC hiding (CSV)
import Data.CSV.Conduit.Conversion
import Control.Monad (mzero)
import qualified Data.Text.Encoding as T
import           Data.Time.Clock (UTCTime)
import           Data.Time.Format
import qualified Data.Vector as V
import qualified Data.ByteString               as B
import qualified Data.ByteString.Internal      as BI
import qualified Data.ByteString.Lazy          as BL
import qualified Data.ByteString.Lazy.Internal as BLI
import qualified Data.ByteString.Lazy.Char8 as C
import Control.Monad.Trans.Class
import Control.Monad.Trans.Maybe
import Data.List.Split


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

data YahooData = YahooData
  { yahooDataDate :: Text
  , yahooDataOpen :: Double
  , yahooDataHigh :: Double
  , yahooDataLow :: Double
  , yahooDataClose :: Double
  , yahooDataAdjClose :: Double
  , yahooDataVolume :: Int
  } deriving (Show, Eq)

instance FromRecord YahooData where
  parseRecord v
    | length v == 6 =
      YahooData <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*>
      v .! 5 <*>
      v .! 6
    | otherwise = mzero

instance ToRecord YahooData where
  toRecord (YahooData yahooDataDate yahooDataOpen yahooDataHigh yahooDataLow yahooDataClose yahooDataAdjClose yahooDataVolume) =
    record
      [ toField yahooDataDate -- toField $ parseTimestamp "%d.%m.%Y %H:%M" yahooDataDate
      , toField yahooDataOpen
      , toField yahooDataHigh
      , toField yahooDataLow
      , toField yahooDataClose
      , toField yahooDataAdjClose
      , toField yahooDataVolume
      ]

readToType :: String -> IO [Parser YahooData]
readToType ticker = do
  yd <- liftIO $ getYahooData ticker
  let charList = lines $ C.unpack yd
  let charListofLists = fmap (splitOn ",") charList
  let bslListofLists = (fmap . fmap) C.pack charListofLists
  let bsListofLists = (fmap . fmap) toStrict1 bslListofLists
  let recordsList = fmap record bsListofLists
  return $ fmap parseRecord recordsList

-- example
printYlist = do
  pl <- readToType "AAPL"
  let yl = fmap runParser pl
  print yl

toStrict1 :: C.ByteString -> B.ByteString
toStrict1 = B.concat . C.toChunks


parseTimestamp ::
     (Monad m, ParseTime t)
  => String -- ^ Format string
  -> String -- ^ Input string
  -> m t
parseTimestamp = parseTimeM True defaultTimeLocale
