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
      [ toField yahooDataDate -- toField $ parseTimestamp "%d.%m.%Y %H:%M" yahooDataDate
      , toField yahooDataOpen
      , toField yahooDataHigh
      , toField yahooDataLow
      , toField yahooDataClose
      , toField yahooDataAdjClose
      , toField yahooDataVolume
      ]

readToType :: IO (Parser YahooData)
readToType = do
  yd <- liftIO $ getYahooData "AAPL"
  let dataList = lines $ C.unpack yd
  let cList = fmap C.pack dataList
  let bList = fmap toStrict1 cList
  let records = record bList
  return $ parseRecord records

printYlist = do
    pl <- readToType
    case runParser pl of
        Left e -> print e
        Right r -> print r

toStrict1 :: C.ByteString -> B.ByteString
toStrict1 = B.concat . C.toChunks


parseTimestamp :: (Monad m, ParseTime t)
               => String   -- ^ Format string
               -> String   -- ^ Input string
               -> m t
parseTimestamp = parseTimeM True defaultTimeLocale
