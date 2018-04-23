{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE BangPatterns        #-}

module Helper.YahooHelper where

import           Control.Concurrent (forkIO)
import           Control.Concurrent.Async (mapConcurrently)
import           Control.Exception as E
import           Control.Lens
import           Control.Monad (mzero)
import           Control.Monad.Except
import qualified Data.ByteString as BB
import qualified Data.ByteString.Lazy.Char8 as C
import           Data.CSV.Conduit
import           Data.CSV.Conduit.Conversion as CSVC
import           Data.Int
import           Data.List.Split
import           Data.Text as T hiding (length, lines, map, splitOn)
import           Data.Text (Text)
import           Data.Time
import           Data.Time.Clock.POSIX
import           Data.Vector ((!))
import           Helper.YahooDB
import           Import hiding (httpLbs, mapConcurrently, newManager)
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import qualified Network.Wreq as W (responseBody, responseStatus, statusCode)
import           System.IO as SIO (appendFile)
import qualified Yadata.LibYahoo as YL

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
      YahooData <$> v .! 0 <*> v .! 1 <*> v .! 2 <*> v .! 3 <*> v .! 4 <*> v .!
      5 <*>
      v .!
      6
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
    x <- parseTimestamp "%Y-%m-%d" (C.unpack (C.fromStrict u))
    pure (YUTCTime x)

readToType :: C.ByteString -> [Parser YahooData]
readToType res = do
  let charList = lines $ C.unpack res
  let charListofLists = fmap (splitOn ",") charList
  let bslListofLists = (fmap . fmap) C.pack charListofLists
  let bsListofLists = (fmap . fmap) toStrict1 bslListofLists
  let recordsList = fmap record bsListofLists
  fmap parseRecord recordsList

onespace :: Text
onespace = " " :: Text

writeYahooLog :: String -> Bool -> IO ()
writeYahooLog s printToFile = do
  now <- getCurrentTime
  _ <- putStrLn $ onespace
      <> T.pack (show now) <> onespace
      <> T.pack s
  when printToFile $ SIO.appendFile "yahoo_.txt" (show now ++ " " ++ s ++ "\r\n")
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

parseTimestamp
  :: (Monad m, ParseTime t)
  => String -- ^ Format string
  -> String -- ^ Input string
  -> m t
parseTimestamp = parseTimeM True defaultTimeLocale

mkCompany :: Vector ByteString -> UTCTime -> Company
mkCompany v now =
  Company
  { companyTitle = decodeUtf8 $ (!) v 1
  , companyWebsite = Just $ decodeUtf8 $ (!) v 6
  , companyDescription = Just $ decodeUtf8 $ (!) v 7
  , companyImage = Nothing
  , companyTicker = decodeUtf8 $ (!) v 0
  , companyGicssector = Just $ decodeUtf8 $ (!) v 2
  , companyGicssubindustry = Just $ decodeUtf8 $ (!) v 3
  , companyCreated = now
  }

insertCompanyIfNotInDB :: Int -> Vector (Vector ByteString) -> IO ()
insertCompanyIfNotInDB vecLen v = do
  now <- liftIO getCurrentTime
  if vecLen > 0
    then do
      let c = mkCompany ((!) v vecLen) now
      insertedCompany <-
        runDBA $ selectFirst [CompanyTicker ==. companyTicker c] []
      case insertedCompany of
        Nothing -> do
          _ <- runDBA $ insert c
          return ()
        Just (Entity cId dbCompany) ->
          case companyWebsite dbCompany of
            Nothing -> do
              writeYahooLog "[COMPANY INSERT] Update company data" False
              _ <-
                runDBA $
                update
                  cId
                  [ CompanyWebsite =. companyWebsite c
                  , CompanyGicssector =. companyGicssector c
                  , CompanyGicssubindustry =. companyGicssubindustry c
                  ]
              return ()
            Just "" -> do
              writeYahooLog "[COMPANY INSERT] Update company data " False
              _ <-
                runDBA $
                update
                  cId
                  [ CompanyWebsite =. companyWebsite c
                  , CompanyGicssector =. companyGicssector c
                  , CompanyGicssubindustry =. companyGicssubindustry c
                  ]
              return ()
            Just _ -> return ()
      insertCompanyIfNotInDB (vecLen - 1) v
      return ()
    else writeYahooLog "[COMPANY INSERT] Company insert finished" False

readCompanyDataFromCSV :: IO ()
readCompanyDataFromCSV = do
  s <- readFile "csvCompanies.csv"
  let v =
        decodeCSV defCSVSettings s :: Either SomeException (Vector (Vector ByteString))
  case v of
    Left _ -> writeYahooLog "[COMPANY INSERT] No file found" False
    Right a -> do
      let vectorLen = length a - 1
      insertCompanyIfNotInDB vectorLen a
      return ()

--------------------------------------------------------------------------------
-- | Part related to doing threaded stuff with yahoo data

readQ :: TChan a -> IO a
readQ = atomically . readTChan

writeQ :: MonadIO m => TChan Text -> Maybe Text -> m ()
writeQ ch v =
  case v of
    Just a  -> atomically $ writeTChan ch a
    Nothing -> return ()

readerThread :: TChan Text -> IO ThreadId
readerThread queue = forkIO loop
  where
    loop = readQ queue >>= putStrLn . (("[THREAD LOG] " :: Text) <>) >> loop

go :: TChan Text -> [Entity Company] -> IO ()
go queue (Entity k c:cs) = do
  _ <- mapConcurrently (yaction queue k) [c] 
  go queue cs
go _ _ = return () 

threader :: IO ()
threader = do
  queue <- atomically $ newTChan
  companies <- liftIO $ runDBA allCompanies
  _ <- readerThread queue
  writeQ queue $ Just "[START]" 
  go queue companies
  writeQ queue $ Just "[END]" 
  return () 

yaction :: (MonadIO m, MonadBase IO m) => TChan Text -> CompanyId -> Company -> m ()
yaction queue cid c = do
  y <- liftIO (getYahoo (companyTicker c))
  threadDelay 10000000
  case y of
    Right a -> do
      _ <- liftIO $ writeQ queue (Just $ "yahoo result for " ++ (companyTitle c))
      let res = readToType a
      let presult = fmap runParser res
      let onlyRights = rights presult
      let historicalList =
            map
              (convertToHistoricalAction
                 cid
                 (companyTicker c))
              onlyRights
      _ <- liftIO $ mapM insertIfNotSaved historicalList
      _ <- liftIO $ writeQ queue (Just $ "inserted yahoo result for " ++ (companyTitle c))
      return ()
    Left e -> do
      _ <- liftIO $ writeQ queue $
             Just $ "error result for " ++
               (companyTitle c) ++
               (T.pack $ show e) 
      return ()

getYahoo :: Text -> IO (Either C.ByteString C.ByteString)
getYahoo ticker = do
  endDate <- liftIO getCurrentTime
  let starDate = UTCTime  (fromGregorian 2000 01 01) 0
  getYahooHisto ticker starDate endDate

getYahooHisto :: Text -> UTCTime -> UTCTime-> IO (Either C.ByteString C.ByteString)
getYahooHisto ticker startDate endDate = do
  manager <- newManager $ managerSetProxy noProxy tlsManagerSettings
  setGlobalManager manager
  cookieRequest <- parseRequest (YL.crumbleLink "KO")
  crumb <-
    E.try (httpLbs cookieRequest manager) :: IO (Either YL.YahooException (Response C.ByteString))
  now <- getCurrentTime
  let crb =
       case crumb of
         Right c -> c
         Left  e -> E.throw e 
  let (jar1, _) = updateCookieJar crb cookieRequest now (createCookieJar [])
  let body = crb ^. W.responseBody
  dataRequest <-
    parseRequest
      (YL.yahooDataLink4TimePeriod
         (T.unpack ticker)
         (C.unpack $ YL.getCrumble body)
         (round (utcTimeToPOSIXSeconds startDate) :: Integer)
         (round (utcTimeToPOSIXSeconds endDate) :: Integer))
  now2 <- getCurrentTime
  let (dataReq, _) = insertCookiesIntoRequest dataRequest jar1 now2
  result <-
    E.try (httpLbs dataReq manager) :: IO (Either YL.YahooException (Response C.ByteString))
  let r =
       case result of
         Right res -> res
         Left  e -> E.throw e 
  let body2 = r ^. W.responseBody
  let status = r ^. W.responseStatus . W.statusCode
  if status == 200
    then return $ Right body2
    else do
      return $ Left body2
