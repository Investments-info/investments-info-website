{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Application
  ( getApplicationDev
  , appMain
  , develMain
  , makeFoundation
  , makeLogWare
    -- * for DevelMain
  , getApplicationRepl
  , shutdownApp
    -- * for GHCI
  , handler
  , db
  , getDbConnectionString
  ) where

import Control.Monad.Logger (liftLoc, runLoggingT)
import Database.Persist.Postgresql
       (createPostgresqlPool, pgConnStr, pgPoolSize, runSqlPool)
import Import
import Language.Haskell.TH.Syntax (qLocation)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp
       (Settings, defaultSettings, defaultShouldDisplayException, getPort,
        setHost, setOnException, setPort)
import Network.Wai.Handler.WarpTLS
import Network.Wai.Middleware.RequestLogger
       (Destination(Logger), IPAddrSource(..), OutputFormat(..),
        destination, mkRequestLogger, outputFormat)
import System.Log.FastLogger
       (defaultBufSize, newStdoutLoggerSet, toLogStr)

import Data.CSV.Conduit
import Data.Vector ((!))
import Control.Concurrent.Async (concurrently_)

import Handler.About
import Handler.Admin
import Handler.Auth
import Handler.Common
import Handler.Company
import Handler.CompanyList
import Handler.Historical
import Handler.Home
import Handler.SearchArticles
import Handler.StoryDetails
import Handler.StoryList
import Handler.CompanyDetails
import Helper.Fixtures as F
import Helper.YahooHelper as YH


mkYesodDispatch "App" resourcesApp

makeFoundation :: AppSettings -> IO App
makeFoundation appSettings = do
    appHttpManager <- newManager
    appLogger <- newStdoutLoggerSet defaultBufSize >>= makeYesodLogger
    appStatic <-
        (if appMutableStatic appSettings then staticDevel else static)
        (appStaticDir appSettings)

    let mkFoundation appConnPool = App {..}
        tempFoundation = mkFoundation $ error "connPool forced in tempFoundation"
        logFunc = messageLoggerSource tempFoundation appLogger

    pool <- flip runLoggingT logFunc $ createPostgresqlPool
        (pgConnStr  $ appDatabaseConf appSettings)
        (pgPoolSize $ appDatabaseConf appSettings)

    runLoggingT (runSqlPool (runMigration migrateAll) pool) logFunc

    return $ mkFoundation pool

makeApplication :: App -> IO Application
makeApplication foundation = do
  logWare <- makeLogWare foundation
  appPlain <- toWaiAppPlain foundation
  return $ logWare $ defaultMiddlewaresNoLogging appPlain

makeLogWare :: App -> IO Middleware
makeLogWare foundation =
  mkRequestLogger
    def
    { outputFormat =
        if appDetailedRequestLogging $ appSettings foundation
          then Detailed True
          else Apache
                 (if appIpFromHeader $ appSettings foundation
                    then FromFallback
                    else FromSocket)
    , destination = Logger $ loggerSet $ appLogger foundation
    }

-- | Warp settings for the given foundation value.
warpSettings :: App -> Settings
warpSettings foundation =
  setPort (appPort $ appSettings foundation) $
  setHost (appHost $ appSettings foundation) $
  setOnException
    (\_req e ->
       when (defaultShouldDisplayException e) $
       messageLoggerSource
         foundation
         (appLogger foundation)
         $(qLocation >>= liftLoc)
         "yesod"
         LevelError
         (toLogStr $ "Exception from Warp: " ++ show e))
    defaultSettings

mkCompany :: Vector ByteString -> UTCTime -> Company
mkCompany v now =
    Company
    { companyTitle = decodeUtf8 $ (!) v 1
    , companyWebsite = Just $ decodeUtf8 $ (!) v 7
    , companyDescription = Just $ decodeUtf8 $ (!) v 7
    , companyImage = Nothing
    , companyTicker = decodeUtf8 $ (!) v 0
    , companyCreated = now
    }

insertCompanyIfNotInDB :: Int -> Vector (Vector ByteString) -> IO ()
insertCompanyIfNotInDB vecLen v = do
    now <- liftIO getCurrentTime
    if vecLen > 0 then
        do
        let c = mkCompany ((!) v (vecLen)) now
        insertedCompany <- runDBA $ selectFirst [CompanyTicker ==. (companyTicker c)] []
        case insertedCompany of
            Nothing -> do
                _ <- runDBA $ insert c
                return ()
            Just _ -> return ()

        insertCompanyIfNotInDB (vecLen - 1) v
        return ()
    else
        print ("Company insert finished" :: Text)

readCompanyDataFromCSV :: IO ()
readCompanyDataFromCSV = do
    s <- readFile "csvCompanies.csv"
    let v = decodeCSV defCSVSettings s :: Either SomeException (Vector (Vector ByteString))
    case v of
        Left _ -> do
            print ("No file found" :: Text)
        Right a -> do
            let vectorLen = (length a) - 1
            insertCompanyIfNotInDB vectorLen a
            return ()

-- | For yesod devel, return the Warp settings and WAI Application.
getApplicationDev :: IO (Settings, Application)
getApplicationDev = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings foundation
  app <- makeApplication foundation
  F.runDeleteAdminsAction
  F.runInsertAdminsAction
  concurrently_ YH.fetchHistoricalData readCompanyDataFromCSV
  return (wsettings, app)

getAppSettings :: IO AppSettings
getAppSettings = loadYamlSettings [configSettingsYml] [] useEnv

-- | main function for use by yesod devel
develMain :: IO ()
develMain = develMainHelper getApplicationDev

tlsS :: TLSSettings
tlsS =
  tlsSettings
    "/etc/letsencrypt/live/investments-info.com/fullchain.pem"
    "/etc/letsencrypt/live/investments-info.com/privkey.pem"

-- | The @main@ function for an executable running this site.
appMain :: IO ()
appMain = do
  settings <- loadYamlSettingsArgs [configSettingsYmlValue] useEnv
  foundation <- makeFoundation settings
  app <- makeApplication foundation
  F.runDeleteAdminsAction
  F.runInsertAdminsAction
  _ <- forkFinally YH.fetchHistoricalData YH.logForkedAction
  -- _ <- forkIO $ YH.fetchHistoricalData foundation
  runTLS tlsS (warpSettings foundation) app

--------------------------------------------------------------
-- Functions for DevelMain.hs (a way to run the app from GHCi)
--------------------------------------------------------------
getApplicationRepl :: IO (Int, App, Application)
getApplicationRepl = do
  settings <- getAppSettings
  foundation <- makeFoundation settings
  wsettings <- getDevSettings $ warpSettings foundation
  app1 <- makeApplication foundation
  return (getPort wsettings, foundation, app1)

shutdownApp :: App -> IO ()
shutdownApp _ = return ()

---------------------------------------------
-- Functions for use in development with GHCi
---------------------------------------------
-- | Run a handler
handler :: Handler a -> IO a
handler h = getAppSettings >>= makeFoundation >>= flip unsafeHandler h

-- | Run DB queries
db :: ReaderT SqlBackend (HandlerT App IO) a -> IO a
db = handler . runDB

getDbConnectionString :: IO ByteString
getDbConnectionString = do
 settings <- getAppSettings
 return $ pgConnStr $ appDatabaseConf settings

{-
 _ <- forkIO $ setupConnectionsToMesh addressText foundation

setupConnectionsToMesh :: Text -> App -> IO ()
setupConnectionsToMesh addressText app = do
  let hints = defaultHints { addrFlags = [AI_NUMERICHOST, AI_NUMERICSERV], addrSocketType = Stream }
      logger = appLogger app
  addr:_ <- getAddrInfo (Just hints) (Just "0.0.0.0") (Just "8585")
  sock   <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

  -- Bind it to the address we're listening to
  attemptBind logger 15 sock (addrAddress addr)

  -- Start listening for connection requests.  Maximum queue size
  -- of 15 connection requests waiting to be accepted.
  listen sock 15
  logStrLn logger "Listening on 8585 for the mesh network"
  -- connect to all known servers in the mesh
  servers <- E.runSqlPool getServers (appConnPool app)

getServers :: SqlPersistT IO [ Entity Servers ]
getServers = do
  selectList [ ServersCategory ==. Just "D3CommandCenter" ] []
-}
