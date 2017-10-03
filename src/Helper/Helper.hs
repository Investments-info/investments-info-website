{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
module Helper.Helper where

import Database.Persist.Sql  (SqlBackend, rawSql, unSingle)
import Data.Text (Text)
import Data.Hashable
import Import
import LibYahoo (getYahooData)
import Data.CSV.Conduit.Conversion as CSVC
import Control.Monad (mzero)
import           Data.Time
import qualified Data.ByteString               as B
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List.Split
import Prelude (show)
import qualified Data.Map.Strict as Map


companyCodes :: [String]
companyCodes = ["KO", "AAPL"]

truncateTables :: MonadIO m => ReaderT SqlBackend m [Text]
truncateTables = do
  result <- rawSql "DELETE FROM 'story';" []
  return (fmap unSingle result)

makeHash :: Hashable a => a -> Int
makeHash s = hash s

 ------------------------------------------------------------------------------------------------------
-- [
-- ("A","Agilent Technologies"),
-- ("AA","Alcoa Corporation"),
-- ("AAC","Aac Holdings"),
-- ("AAN","Aaron's Inc"),
-- ("AAP","Advanced Auto Parts Inc")
-- ]
newtype TickerList = TickerList []
type YDataDate = UTCTime
type YDataOpen = Double
type YDataHigh = Double
type YDataLow = Double
type YDataClose = Double
type YDataAdjClose = Double
type YDataVolume = Int

data YahooData = YahooData
  { yahooDataDate :: !YDataDate
  , yahooDataOpen :: !YDataOpen
  , yahooDataHigh :: !YDataHigh
  , yahooDataLow :: !YDataLow
  , yahooDataClose :: !YDataClose
  , yahooDataAdjClose :: !YDataAdjClose
  , yahooDataVolume :: !YDataVolume
  } deriving (Show, Eq)


data GYData a where
  YData
    :: YDataDate
    -> YDataOpen
    -> YDataHigh
    -> YDataLow
    -> YDataClose
    -> YDataAdjClose
    -> YDataVolume
    -> GYData a
  deriving (Show, Eq)

instance FromRecord (GYData a) where
  parseRecord v
    | length v == 7 =
      YData <$>   v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*>
      v .! 5 <*>
      v .! 6
    | otherwise = mzero

instance ToRecord (GYData a) where
  toRecord (YData yDataDate yDataOpen yDataHigh yDataLow yDataClose yDataAdjClose yDataVolume) =
    record
      [ toField (show yDataDate)
      , toField yDataOpen
      , toField yDataHigh
      , toField yDataLow
      , toField yDataClose
      , toField yDataAdjClose
      , toField yDataVolume
      ]

instance FromRecord YahooData  where
  parseRecord v
    | length v == 7 =
      YahooData <$>   v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*>
      v .! 5 <*>
      v .! 6
    | otherwise = mzero

instance ToRecord YahooData where
  toRecord (YahooData yahooDataDate yahooDataOpen yahooDataHigh yahooDataLow yahooDataClose yahooDataAdjClose yahooDataVolume) =
    record
      [ toField (show yahooDataDate)
      , toField yahooDataOpen
      , toField yahooDataHigh
      , toField yahooDataLow
      , toField yahooDataClose
      , toField yahooDataAdjClose
      , toField yahooDataVolume
      ]

instance FromField UTCTime where
    parseField u = do
        x <- parseTimestamp "%Y-%m-%d" (C.unpack(C.fromStrict u))
        pure x

readToList :: MonadIO m => String -> m ()
readToList ticker = do
  yd <- liftIO $ getYahooData ticker
  let charList = lines $ C.unpack yd
  let charListofLists = fmap (splitOn ",") charList
  let bslListofLists = (fmap . fmap) C.pack charListofLists
  let bsListofLists = (fmap . fmap) toStrict1 bslListofLists
  print bsListofLists

readToType :: String -> IO [Parser (GYData a)]
readToType ticker = do
  yd <- liftIO $ getYahooData ticker
  let charList = lines $ C.unpack yd
  let charListofLists = fmap (splitOn ",") charList
  let bslListofLists = (fmap . fmap) C.pack charListofLists
  let bsListofLists = (fmap . fmap) toStrict1 bslListofLists
  let recordsList = fmap record bsListofLists
  return $ fmap parseRecord recordsList

printYlist :: IO ()
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
