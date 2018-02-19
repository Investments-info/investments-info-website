{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}

module Helper.YahooHelper
  ( fetchHistoricalData
  , logForkedAction
  , writeYahooLog
  , YahooException(..)
  ) where

import           Control.Exception as E
import           Control.Lens
import           Control.Monad (mzero)
import           Control.Monad.Except ()
import           Control.Monad.Except
import qualified Data.ByteString as BB
import           Data.ByteString.Lazy as B (ByteString, drop, take)
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.CSV.Conduit.Conversion as CSVC
import           Data.Int
import           Data.List.Split
import           Data.Text as T hiding (length, lines, map, splitOn)
import           Data.Time
import           Data.Typeable
import           Helper.YahooDB
import           Import hiding (httpLbs, newManager)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
-- import Network.HTTP.Simple hiding (httpLbs)
import qualified Network.Wreq as W (responseBody, responseStatus, statusCode)
import           System.IO as SIO (appendFile)
import           Text.Regex.PCRE

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
  show YStatusCodeException    = "Yadata :: data fetch exception!"
  show YCookieCrumbleException = "Yadata :: cookie crumble exception!"
  show YWrongTickerException   = "Yadata :: wrong ticker passed in!"

instance Exception YahooException

type YDataDate = YUTCTime
type YDataOpen = Double
type YDataHigh = Double
type YDataLow = Double
type YDataClose = Double
type YDataAdjClose = Double
type YDataVolume = Int

data YahooData = YahooData
  { yahooDataDate     :: !YDataDate
  , yahooDataOpen     :: !YDataOpen
  , yahooDataHigh     :: !YDataHigh
  , yahooDataLow      :: !YDataLow
  , yahooDataClose    :: !YDataClose
  , yahooDataAdjClose :: !YDataAdjClose
  , yahooDataVolume   :: !YDataVolume
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

newtype YUTCTime = YUTCTime
    { getYtime :: UTCTime
    } deriving (Eq, Show)

instance FromField YUTCTime where
    parseField u = do
        x <- parseTimestamp "%Y-%m-%d" (C.unpack(C.fromStrict u))
        pure (YUTCTime x)


getYahooData :: Text -> ExceptT YahooException IO C.ByteString
getYahooData ticker = ExceptT $ do
  manager <- newManager $ managerSetProxy noProxy tlsManagerSettings
  setGlobalManager manager
  cookieRequest <- parseRequest (crumbleLink "KO")
  crumb <-
    E.try (httpLbs cookieRequest manager) :: IO (Either YahooException (Response C.ByteString))
  case crumb of
    Left _ -> do
      writeYahooLog $ "[YAHOO ERR] cookieRequest received Left result "
      writeYahooLog $ "[YAHOO ERR]  " ++ (show YCookieCrumbleException)
      return $ Left YCookieCrumbleException
    Right crb -> do
      writeYahooLog $ "[YAHOO] cookieRequest received Right result "
      now <- getCurrentTime
      let (jar1, _) = updateCookieJar crb cookieRequest now (createCookieJar [])
      let body = crb ^. W.responseBody
      dataRequest <-
        parseRequest
          (yahooDataLink (T.unpack ticker) (C.unpack $ getCrumble body))
      now2 <- getCurrentTime
      let (dataReq,_) = insertCookiesIntoRequest dataRequest jar1 now2
      result <-
        E.try (httpLbs dataReq manager) :: IO (Either YahooException (Response C.ByteString))
      case result of
        Left _ -> do
          writeYahooLog $  "[YAHOO ERR] yahooDataRequest received Left result "
          writeYahooLog $  ("[YAHOO ERR]  " ++ show YStatusCodeException)
          return $ Left YStatusCodeException
        Right d -> do
          writeYahooLog $ "[YAHOO] yahooDataRequest received Right result "
          let body2 = d ^. W.responseBody
          let status = d ^. W.responseStatus . W.statusCode
          if status == 200
            then return $ Right $ body2
            else do
              writeYahooLog $ "[YAHOO ERR] yahooDataRequest status code was not 200"
              writeYahooLog $  ("[YAHOO ERR]  " ++ show YStatusCodeException)
              writeYahooLog $  ("[YAHOO ERR]  " ++ show body2)
              return $ Left YStatusCodeException


readToType :: Text -> ExceptT String IO [Parser YahooData]
readToType ticker = ExceptT $ do
  res <- runExceptT $ getYahooData ticker
  case res of
    Left _ -> do
         writeYahooLog $  "[YAHOO ERR] readToType received Left result "
         writeYahooLog $  ("[YAHOO ERR]  " ++ show YStatusCodeException)
         return $ Left $ show YStatusCodeException
    Right yd -> do
      writeYahooLog $ "[YAHOO] readToType received Right result "
      let charList = lines $ C.unpack yd
      let charListofLists = fmap (splitOn ",") charList
      let bslListofLists = (fmap . fmap) C.pack charListofLists
      let bsListofLists = (fmap . fmap) toStrict1 bslListofLists
      let recordsList = fmap record bsListofLists
      let result = fmap parseRecord recordsList
      return $ Right result

saveCompanyData :: Entity Company -> IO ()
saveCompanyData companyE = do
  let cid = entityKey companyE
  let company = entityVal companyE
  pl <- runExceptT $ readToType (companyTicker company)
  case pl of
    Left _ -> liftIO $ return ()
    Right res -> do
      let result = fmap runParser res
      let onlyRights = rights result
      let historicalList = (map (convertToHistoricalAction cid (companyTicker company)) onlyRights)
      _ <- liftIO $ mapM insertIfNotSaved historicalList
      return ()

writeYahooLog :: String -> IO ()
writeYahooLog s = do
    now <- getCurrentTime
    SIO.appendFile "yahoo_.txt" ((show now) ++ " " ++ s ++ "\r\n")
    return ()

convertToHistoricalAction :: CompanyId -> Text -> YahooData -> Historical
convertToHistoricalAction cid ticker YahooData {..} =
  Historical
  { historicalCompanyId = cid
  , historicalTicker = ticker
  , historicalRecordDate = getYtime yahooDataDate
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

fetchHistoricalData :: IO ()
fetchHistoricalData = do
    companies <- liftIO $ runDBA $ allCompanies
    _ <- traverse saveCompanyData companies
    return ()

logForkedAction :: (Show a, Exception e) => Either e a -> IO ()
logForkedAction (Left x)  = print x
logForkedAction (Right x) = print x
