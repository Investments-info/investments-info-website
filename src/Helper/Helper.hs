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
import Prelude (read, show)
import Control.Monad.Trans.Maybe

companyCodes :: [String]
companyCodes = ["KO", "AAPL"]

truncateTables :: MonadIO m => ReaderT SqlBackend m [Text]
truncateTables = do
  result <- rawSql "DELETE FROM 'story';" []
  return (fmap unSingle result)

makeHash :: Hashable a => a -> Int
makeHash s = hash s

 ------------------------------------------------------------------------------------------------------

data YahooData = YahooData
  { yahooDataDate :: !Double
  , yahooDataOpen :: !Double
  , yahooDataHigh :: !Double
  , yahooDataLow :: !Double
  , yahooDataClose :: !Double
  , yahooDataAdjClose :: !Double
  , yahooDataVolume :: !Int
  } deriving (Show, Eq)

data GYahooData a  where
    YData ::  UTCTime -> Double -> Double -> Double -> Double -> Double ->  Int -> GYahooData a deriving (Show, Eq)

instance FromRecord (GYahooData a) where
  parseRecord v
    | length v == 7 =
      YData <$>   v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*>
      v .! 5 <*>
      v .! 6
    | otherwise = mzero

instance ToRecord (GYahooData a) where
  toRecord (YData yahooDataDate yahooDataOpen yahooDataHigh yahooDataLow yahooDataClose yahooDataAdjClose yahooDataVolume) =
    record
      [ toField (show yahooDataDate)
      , toField yahooDataOpen
      , toField yahooDataHigh
      , toField yahooDataLow
      , toField yahooDataClose
      , toField yahooDataAdjClose
      , toField yahooDataVolume
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

readToList ticker = do
  yd <- liftIO $ getYahooData ticker
  let charList = lines $ C.unpack yd
  let charListofLists = fmap (splitOn ",") charList
  let bslListofLists = (fmap . fmap) C.pack charListofLists
  let bsListofLists = (fmap . fmap) toStrict1 bslListofLists
  print bsListofLists

readToType :: String -> IO [Parser (GYahooData a)]
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

-- fromParser :: (Monad m, ParseTime t) => B.ByteString -> MaybeT  a UTCTime
-- fromParser p =  MaybeT (parseTimestamp "%d %m %Y" (C.unpack(C.fromStrict p)))
