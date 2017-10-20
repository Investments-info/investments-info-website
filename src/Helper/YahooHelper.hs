{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Helper.YahooHelper where

import Conduit
import Control.Lens
import Control.Monad.Reader
import Control.Monad.Trans.Class
import Control.Monad.Trans.Resource (runResourceT)
import Data.Aeson.Lens
import qualified Data.ByteString as BB
import Data.ByteString (ByteString)
import Data.ByteString.Lazy as B (ByteString, drop, take)
import qualified Data.ByteString.Lazy.Char8 as C
import Data.CSV.Conduit
import Data.CSV.Conduit.Conversion as CSVC
import Data.Conduit (($$))
import qualified Data.Conduit.Binary as CB
import qualified Data.Conduit.List as CL
import Data.Int
import Data.List.Split
import Data.Text as T hiding (length, lines, map, splitOn)
import Data.Time
import Data.Typeable
import Data.Void
import Helper.YahooDB
import Import hiding (httpLbs, newManager)
import Network.Connection (TLSSettings(..))
import Network.HTTP.Client.Conduit hiding (httpLbs)
import Network.HTTP.Client.TLS
       (setGlobalManager, tlsManagerSettings)
import Network.HTTP.Simple hiding (withResponse)
import Network.HTTP.Types.Status (statusCode)
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

csvProcessor :: ConduitM Import.ByteString Void (ReaderT Manager IO) () -> Record
csvProcessor = undefined --toRecord

streamYahooData :: Request -> IO ()
streamYahooData req = do
    withManagerSettings defaultManagerSettings $ do
        (withResponse req $ \resp -> do
            runConduit $
                let httpConduit = responseBody resp
                in httpConduit .| csvProcessor)
          `catch` \(e :: HttpException) -> do
            -- lift $ putStrLn "Sad :("
            throw e

getYahooData :: Text -> IO ()
getYahooData ticker = do
    cookieRequest <- parseRequest (crumbleLink "KO")
    crumb <- httpLbs cookieRequest
    now <- getCurrentTime
    let (jar1, _) = updateCookieJar crumb cookieRequest now (createCookieJar [])
    let body = responseBody crumb
    dataRequest <- parseRequest (yahooDataLink (T.unpack ticker) (C.unpack $ getCrumble body))
    now2 <- getCurrentTime
    let (dataReq,_) = insertCookiesIntoRequest dataRequest jar1 now2
    streamYahooData dataReq

getErrorMessage json = json ^? key "chart" . key "error" . _String

-- readToType :: Text -> IO (Either String [Parser YahooData])
-- readToType ticker = do
--   res <- getYahooData ticker
--   let charList = lines $ C.unpack res
--   let charListofLists = fmap (splitOn ",") charList
--   let bslListofLists = (fmap . fmap) C.pack charListofLists
--   let bsListofLists = (fmap . fmap) toStrict1 bslListofLists
--   let recordsList = fmap record bsListofLists
--   let result = fmap parseRecord recordsList
--   return $ Right result

-- saveCompanyData :: Entity Company -> IO ()
-- saveCompanyData companyE = do
--   let cid = entityKey companyE
--   let company = entityVal companyE
--   pl <- liftIO $ readToType (companyTicker company)
--   case pl of
--     Left _ -> liftIO $ return ()
--     Right res -> do
--       let result = fmap runParser res
--       let onlyRights = rights result
--       let historicalList = (map (convertToHistoricalAction cid (companyTicker company)) onlyRights)
--       _ <- liftIO $ mapM insertIfNotSaved historicalList
--       return ()


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

-- fetchHistoricalData :: IO ()
-- fetchHistoricalData = do
--     companies <- liftIO $ runDBA $ allCompanies
--     _ <- traverse saveCompanyData companies
--     return ()
