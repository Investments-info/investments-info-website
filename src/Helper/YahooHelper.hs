{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Helper.YahooHelper
  ( fetchHistoricalData
  , logForkedAction
  , YahooException(..)
  ) where

import Control.Exception as E
import Control.Lens
import Control.Monad (mzero)
import Control.Monad.Except
import qualified Data.ByteString as BB
import qualified Data.ByteString.Lazy as B
       (ByteString, concat, drop, pack, take)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.CSV.Conduit.Conversion as CSVC
import Data.Hashable
import Data.Int
import Data.List.Split
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text as T hiding (length, lines, map, splitOn)
import Data.Time
import Data.Time.Clock
import Data.Typeable
import Helper.YahooDB
import Import hiding (httpLbs, newManager)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Simple hiding (httpLbs)
import qualified Network.Wreq as W
       (responseBody, responseStatus, statusCode)
import Text.Regex.PCRE

crumbleLink :: String -> String
crumbleLink ticker =
  "https://finance.yahoo.com/quote/" ++ ticker ++ "/history?p=" ++ ticker

yahooDataLink :: String -> String -> String
yahooDataLink ticker crumb =
  "https://query1.finance.yahoo.com/v7/finance/download/" ++
  ticker ++
  "?period1=1201686274&period2=1504364674&interval=1d&events=history&crumb=" ++
  crumb

crumblePattern :: String
crumblePattern = "CrumbStore\":{\"crumb\":\"(.*?)\"}"

getCrumble :: B.ByteString -> B.ByteString
getCrumble crumbText = do
  let test = crumbText =~ crumblePattern :: (Int, Int)
  B.take
    (fromIntegral (snd test) - 24)
    (B.drop (fromIntegral (fst test) + 22) crumbText)


------------------------------------------------------------------------------------------------------

data YahooException
  = YStatusCodeException
  | YCookieCrumbleException
  | YWrongTickerException
  deriving (Typeable)

instance Show YahooException where
  show YStatusCodeException		= "Yadata :: data fetch exception!"
  show YCookieCrumbleException	= "Yadata :: cookie crumble exception!"
  show YWrongTickerException	= "Yadata :: wrong ticker passed in!"

instance Exception YahooException

newtype TickerList = TickerList [String]
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


getYahooData :: Text -> IO (Either YahooException C.ByteString)
getYahooData ticker = do
  manager <- newManager $ managerSetProxy noProxy tlsManagerSettings
  setGlobalManager manager
  cookieRequest <- parseRequest (crumbleLink "KO")
  crumb <-
    E.try (httpLbs cookieRequest manager) :: IO (Either YahooException (Response C.ByteString))
  case crumb of
    Left e -> do
      $(logError) $ T.pack "::cookieRequest received Left result "
      return $ Left YCookieCrumbleException
    Right crb -> do
      $(logDebug) $ T.pack "::cookieRequest received Right result "
      now <- getCurrentTime
      let (jar1, _) = updateCookieJar crb cookieRequest now (createCookieJar [])
      let body = crb ^. W.responseBody
      dataRequest <-
        parseRequest
          (yahooDataLink (T.unpack ticker) (C.unpack $ getCrumble body))
      now2 <- getCurrentTime
      let (dataReq, jar2) = insertCookiesIntoRequest dataRequest jar1 now2
      result <-
        E.try (httpLbs dataReq manager) :: IO (Either YahooException (Response C.ByteString))
      case result of
        Left e -> do
          $(logError) $ T.pack "::yahooDataRequest received Left result "
          return $ Left YStatusCodeException
        Right d -> do
          $(logDebug) $ T.pack "::yahooDataRequest received Right result "
          let body2 = d ^. W.responseBody
          let status = d ^. W.responseStatus . W.statusCode
          if status == 200
            then return $ Right $ body2
            else do
              $(logError) $ T.pack "::yahooDataRequest status code was not 200"
              return $ Left YStatusCodeException


readToType :: Text -> IO (Either String [Parser YahooData])
readToType ticker = do
  res <- getYahooData ticker
  case res of
    Left err -> do
        $(logError) $ T.pack "::readToType received Left result "
        return $ Left $ show YStatusCodeException
    Right yd -> do
      $(logDebug) $ T.pack "::readToType received Right result "
      let charList = lines $ C.unpack yd
      let charListofLists = fmap (splitOn ",") charList
      let bslListofLists = (fmap . fmap) C.pack charListofLists
      let bsListofLists = (fmap . fmap) toStrict1 bslListofLists
      let recordsList = fmap record bsListofLists
      let result = fmap parseRecord recordsList
      return $ Right result

saveCompanyData :: CompanyId -> Text -> IO ()
saveCompanyData cid ticker = do
  pl <- liftIO $ readToType ticker
  case pl of
    Left e -> liftIO $ return ()
    Right res -> do
      let result = fmap runParser res
      let onlyRights = rights result
      let historicalList = (map (convertToHistoricalAction cid ticker) onlyRights)
      _ <- liftIO $ mapM insertIfNotSaved historicalList
      return ()


convertToHistoricalAction :: CompanyId -> Text -> YahooData -> Historical
convertToHistoricalAction cid ticker YahooData {..} =
  Historical
  { historicalCompanyId = cid
  , historicalTicker = ticker
  , historicalRecordDate = yahooDataDate
  , historicalRecordOpen = yahooDataOpen
  , historicalRecordHigh = yahooDataHigh
  , historicalRecordLow = yahooDataLow
  , historicalRecordClose = yahooDataClose
  , historicalRecordAdjClose = yahooDataAdjClose
  , historicalRecordVolume = yahooDataVolume
  }


toStrict1 :: C.ByteString -> BB.ByteString
toStrict1 = BB.concat . C.toChunks

parseTimestamp ::
     (Monad m, ParseTime t)
  => String -- ^ Format string
  -> String -- ^ Input string
  -> m t
parseTimestamp = parseTimeM True defaultTimeLocale

--------------------------------------------
-- YAHOO
-------------------------------------------
getAllCompanies = runDBA $ allCompanies

fetchHistoricalData :: IO ()
fetchHistoricalData = do
    companies <- runDBA $ allCompanies
    undefined
    -- TODO find a function that does map and applyes two param function to list
    -- map (saveCompanyData  companyId companyTicker) companies

logForkedAction :: (Show a, Exception e) => Either e a -> IO ()
logForkedAction (Left x) = print x
logForkedAction (Right x) = print x
