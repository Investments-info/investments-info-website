{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module Helper.YahooHelper where

import Control.Exception as E
import Control.Lens
import Control.Monad (mzero)
import Control.Monad.Except
import qualified Data.ByteString as BB
import Data.ByteString.Lazy as B
       (ByteString, drop, take)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.CSV.Conduit.Conversion as CSVC
import Data.Int
import Data.List.Split
import Data.Text as T hiding (length, lines, map, splitOn)
import Data.Time
import Data.Typeable
import Helper.YahooDB
import Import hiding (httpLbs, newManager, withManagerSettings)
import Network.Connection (TLSSettings (..))
import Network.HTTP.Conduit
import Control.Monad.Trans.Resource (runResourceT)
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
  show YStatusCodeException     = "Yadata :: data fetch exception!"
  show YCookieCrumbleException  = "Yadata :: cookie crumble exception!"
  show YWrongTickerException    = "Yadata :: wrong ticker passed in!"

instance Exception YahooException

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


parseTimestamp ::
     (Monad m, ParseTime t)
  => String -- ^ Format string
  -> String -- ^ Input string
  -> m t
parseTimestamp = parseTimeM True defaultTimeLocale

toStrict1 :: C.ByteString -> BB.ByteString
toStrict1 = BB.concat . C.toChunks

simpleHTTPWithUserAgent :: String -> IO ()
simpleHTTPWithUserAgent url = do
    r  <- parseUrl url
    let request = r {requestHeaders = [("User-Agent","HTTP-Conduit")]}
    let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
    res <- withManagerSettings settings $ httpLbs request
    print res

-- getYahooData :: Text -> ReaderT Manager C.ByteString
getYahooData ticker = do
  let settings = mkManagerSettings (TLSSettingsSimple True False False) Nothing
  cookieRequest <- parseRequest (crumbleLink "KO")
  res <- withManagerSettings settings $ httpLbs cookieRequest
  now <- getCurrentTime
  let (jar1, _) = updateCookieJar crb cookieRequest now (createCookieJar [])
  let body = crb -- ^. W.responseBody
  dataRequest <-
        parseRequest
          (yahooDataLink (T.unpack ticker) (C.unpack $ getCrumble body))
  now2 <- getCurrentTime
  let (dataReq,_) = insertCookiesIntoRequest dataRequest jar1 now2
  result <- withManagerSettings settings $ httpLbs  dataReq
  let body2 = d -- ^. W.responseBody
  let status = d -- ^. W.responseStatus . W.statusCode
  if status == 200
     then return $ Right $ body2
     else return $ Left []

{-

readToType :: Text -> IO (Either String [Parser YahooData])
readToType ticker = do
  res <- getYahooData ticker
  case res of
    Left _ -> do
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

saveCompanyData :: Entity Company -> IO ()
saveCompanyData companyE = do
  let cid = entityKey companyE
  let company = entityVal companyE
  pl <- liftIO $ readToType (companyTicker company)
  case pl of
    Left _ -> liftIO $ return ()
    Right res -> do
      let result = fmap runParser res
      let onlyRights = rights result
      let historicalList = (map (convertToHistoricalAction cid (companyTicker company)) onlyRights)
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




fetchHistoricalData :: IO ()
fetchHistoricalData = do
    companies <- liftIO $ runDBA $ allCompanies
    _ <- traverse saveCompanyData companies
    return ()

-}
