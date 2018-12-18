{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module Model
  ( module Import
  , module Model
  ) where

import           Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import           Control.Monad.Trans.Control (MonadBaseControl)
import           Data.ByteString.Char8 (pack)
import           Data.Time (UTCTime, getCurrentTime)
import           Data.Yaml
import           Database.Esqueleto
import qualified Database.Persist as P
import           Database.Persist.Postgresql (ConnectionString, withPostgresqlPool)
import           Database.Persist.Sqlite (runSqlite)
import           Database.Persist.TH
import           Model.BCrypt as Import
import           System.Environment (getEnv)
import           Universum hiding (on, (^.))

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User json sql=users
    email Text
    name Text Maybe
    lastname Text Maybe
    image Text Maybe
    country Text Maybe
    town Text Maybe
    newsletter Int Maybe
    created_at UTCTime default=current_timestamp
    UniqueUserEmail email
    deriving Eq Show Typeable

Password sql=passwords
  hash BCrypt
  user UserId
  UniquePasswordUser user
  deriving Eq Show

Story
    hashId Int
    title Text
    link Text
    content Text Maybe
    image Text Maybe
    created UTCTime default=current_timestamp
    deriving Eq
    deriving Show

Admin sql=admins
  account UserId
  UniqueAdminUser account
  deriving Eq Show

Company json
    title Text
    website Text Maybe
    description Text Maybe
    image Text Maybe
    ticker Text
    gicssector Text Maybe
    gicssubindustry Text Maybe
    created UTCTime default=current_timestamp
    deriving Eq
    deriving Show

Historical json
    companyId CompanyId
    ticker Text
    recordDate UTCTime
    recordOpen Double
    recordHigh Double
    recordLow Double
    recordClose Double
    recordAdjClose Double
    recordVolume Int
    deriving Eq
    deriving Show
|]

type ControlIO m = (MonadIO m, MonadBaseControl IO m)

type DBM m a =
  (ControlIO m, MonadThrow m, Monad m) => SqlPersistT m a

type DB a = forall m. DBM m a

type DBVal val =
  ( PersistEntity val
  , PersistEntityBackend val ~ SqlBackend
  , PersistStore (PersistEntityBackend val))

instance ToJSON (Entity Story) where
    toJSON (Entity _ p) = object
        [ "title"   .= storyTitle p
        , "link"    .= storyLink p
        , "content" .= storyContent p
        , "image"   .= storyImage p
        ]

data DBConfig = DBConfig
  { dbhost :: Text
  , dbdatabase :: Text
  , dbuser :: Text
  , dbpassword :: Text
  , dbport :: Text
  } deriving (Show, Generic)

instance FromJSON DBConfig

-------------------------------------------------------
-- create
-------------------------------------------------------

createUser :: Text -> Text -> DB (Entity User)
createUser email password = do
  now <- liftIO getCurrentTime
  let newUser = User email Nothing Nothing Nothing Nothing Nothing Nothing now
  user <- insertBy newUser
  case user of
    Left (Entity userId _) -> insertHashed password userId newUser
    Right userId -> insertHashed password userId newUser

createUserForNewsletter :: Text -> Text -> Maybe Int -> DB (Entity User)
createUserForNewsletter email password newsletter = do
  now <- liftIO getCurrentTime
  let newUser = User email Nothing Nothing Nothing Nothing Nothing newsletter now
  user <- insertBy newUser
  case user of
    Left (Entity userId _) -> insertHashed password userId newUser
    Right userId  -> insertHashed password userId newUser

insertHashed :: Text -> Key User -> User -> DB (Entity User)
insertHashed password userId newUser = do
  hash <- liftIO $ hashPassword password
  void $ insertBy $ Password hash userId
  return (Entity userId newUser)

setUserForNewsletter :: Maybe Int -> UserId -> DB (Key User)
setUserForNewsletter newsletter userId = do
  P.update userId [UserNewsletter P.=. newsletter]
  return userId

createAdmin :: Key User -> DB (Either (Entity Admin) AdminId)
createAdmin userKey = do
  let newAdmin = Admin userKey
  insertBy newAdmin

createCompany :: Text -> Text -> Text -> Text -> Text -> Text -> Text -> DB (Entity Company)
createCompany title website description image ticker gicssector gicssubindustry = do
  now <- liftIO getCurrentTime
  let newCompany = Company title (Just website) (Just description) (Just image) ticker (Just gicssector) (Just gicssubindustry) now
  companyId <- insert newCompany
  return (Entity companyId newCompany)

---------------------------------------------------------
-- get
---------------------------------------------------------

getUserPassword :: Text -> DB (Maybe (Entity User, Entity Password))
getUserPassword email = fmap listToMaybe $
  select $
  from $ \(u `InnerJoin` p) -> do
  on (u ^. UserId ==. p ^. PasswordUser)
  where_ (u ^. UserEmail ==. val email)
  return (u, p)

getUserEntity :: Text -> DB (Maybe (Entity User))
getUserEntity email = fmap listToMaybe $
  select $
  from $ \user -> do
  where_ (user ^. UserEmail ==. val email)
  return user

getUserForNewsletter :: Text -> DB (Maybe (Entity User))
getUserForNewsletter email = fmap listToMaybe $
  select $
  from $ \user -> do
  where_ (user ^. UserEmail ==. val email)
  -- where_ (user ^. UserNewsletter ==. val (Just True))
  return user

allCompanies :: DB [Entity Company]
allCompanies =
  select $
    from $ \c ->
    return c

getCompanyById :: CompanyId -> DB (Maybe(Entity Company))
getCompanyById cid = fmap listToMaybe $
  select $
  from $ \c -> do
  where_ (c ^. CompanyId ==. val cid)
  return c

getAllCompanyHistoricalDataById :: CompanyId -> DB [Entity Historical]
getAllCompanyHistoricalDataById cid =
  select $
  from $ \c -> do
  where_ (c ^. HistoricalCompanyId ==. val cid)
  where_ (c ^. HistoricalRecordOpen Database.Esqueleto.>. val 1)
  orderBy [desc (c ^. HistoricalRecordDate)]
  limit 1000
  return c

getLatestUniqueStories :: DB [Entity Story]
getLatestUniqueStories =
  select $
    from $ \story -> do
    groupBy (story ^. StoryId, story ^. StoryHashId)
    orderBy [desc (story ^. StoryCreated)]
    limit 10
    return story


--------------------------------------------------------
-- delete
--------------------------------------------------------

deleteAllCompanies :: DB Int64
deleteAllCompanies =
  Database.Esqueleto.deleteCount $ from $ \(_ :: SqlExpr (Entity Company)) -> pass

deleteAdminUsers :: Text -> DB ()
deleteAdminUsers email = do
  mUser <- selectFirst [UserEmail P.==. email] []
  case mUser of
    Nothing -> pass
    Just u ->
      Database.Esqueleto.delete $
         from $ \p  -> do
         where_ (p ^. AdminAccount  ==. val (entityKey u))
         pass

deleteAdminPasswords :: Text -> DB ()
deleteAdminPasswords email = do
  mUser <- selectFirst [UserEmail P.==. email] []
  case mUser of
    Nothing -> pass
    Just u ->
      Database.Esqueleto.delete $
        from $ \p -> do
        where_ (p ^. PasswordUser ==. val (entityKey u))
        pass

deleteUserAdmins :: Text -> DB Int64
deleteUserAdmins email =
  Database.Esqueleto.deleteCount $
  from $ \u -> where_ (u ^. UserEmail ==. val email)

--------------------------------------------------------
-- count
--------------------------------------------------------

countUsersByEmail :: Text -> DB Int
countUsersByEmail email = do
  (cnt:_) :: [Database.Esqueleto.Value Int] <-
    select .
     from $ \u -> do
     where_ (u ^. UserEmail ==. val email)
     return countRows
  return $ unValue cnt

getCompanyCount :: IO (Database.Esqueleto.Value Int)
getCompanyCount = do
  (companies:_) :: [Database.Esqueleto.Value Int] <-
    runDBA $
    select .
    from $ \(_ :: SqlExpr (Entity Company)) ->
      return countRows
  return companies

getUserCount :: IO (Database.Esqueleto.Value Int)
getUserCount = do
  (users:_) :: [Database.Esqueleto.Value Int] <-
    runDBA $
    select .
    from $ \(_ :: SqlExpr (Entity User)) ->
      return countRows
  return users

getArticleCount :: IO (Database.Esqueleto.Value Int)
getArticleCount = do
  (articles:_) :: [Database.Esqueleto.Value Int] <-
    runDBA $
    select .
    from $ \(_ :: SqlExpr (Entity Story)) ->
      return countRows
  return articles

getHistoryCount :: IO (Database.Esqueleto.Value Int)
getHistoryCount = do
  (history:_) :: [Database.Esqueleto.Value Int] <-
    runDBA $
    select .
    from $ \(_ :: SqlExpr (Entity Historical)) ->
      return countRows
  return history

--------------------------------------------------------
-- run actions
--------------------------------------------------------

dumpMigration :: DB ()
dumpMigration = printMigration migrateAll

runMigrations :: DB ()
runMigrations = runMigration migrateAll

runDBSqlite :: DB a -> IO a
runDBSqlite = runSqlite "investments-info.sqlite3"

devConn :: IO ConnectionString
devConn = do
  cs <- readConfig
  return $ encodeUtf8 cs

runDBA :: DB a -> IO a
runDBA a = do
  conn <- getEnv "iiservant"
  -- conn <- devConn
  runNoLoggingT $
    withPostgresqlPool (pack conn) 10
      $ \pool -> liftIO $ runSqlPersistMPool a pool

runDevDBV :: DB a -> IO a
runDevDBV a = do
  conn <- devConn
  runStdoutLoggingT $
    withPostgresqlPool conn 10
      $ \pool -> liftIO $ runSqlPersistMPool a pool

readConfig :: IO Text
readConfig = do
  cnf <- decodeFile "config/settings.yml" :: IO (Maybe DBConfig)
  case cnf of
    Nothing -> error "Could not read database credentials!"
    Just DBConfig {..} ->
      return $
      "dbname=" <> dbdatabase <> " host=" <> dbhost <> " user=" <> dbuser <>
      " password=" <>
      dbpassword <>
      " port=" <>
      dbport
