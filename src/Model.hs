{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RecordWildCards            #-}
module Model
  ( module Import
  , module Model
  ) where

import Control.Monad.Logger (runNoLoggingT, runStdoutLoggingT)
import ClassyPrelude.Yesod hiding ((==.), hash, on)
import Data.Maybe (listToMaybe)
import Database.Esqueleto
import qualified Database.Persist as P
import Database.Persist.Sqlite (runSqlite)
import Model.BCrypt as Import
import Model.Instances as Import ()
import Database.Persist.Postgresql (ConnectionString, withPostgresqlPool)

type ControlIO m = (MonadIO m, MonadBaseControl IO m)

type DBM m a =
  (ControlIO m, MonadThrow m, Monad m) => SqlPersistT m a

type DB a = forall m. DBM m a

type DBVal val =
  ( PersistEntity val
  , PersistEntityBackend val ~ SqlBackend
  , PersistStore (PersistEntityBackend val))


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
    email Text
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
Company
    title Text
    website Text Maybe
    description Text Maybe
    image Text Maybe
    ticker Text
    created UTCTime default=current_timestamp
    deriving Eq
    deriving Show
Historical
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

instance ToJSON (Entity Story) where
    toJSON (Entity _ p) = object
        [ "title"   .= storyTitle p
        , "link"    .= storyLink p
        , "content" .= storyContent p
        , "image"   .= storyImage p
        ]

getUserPassword :: Text -> DB (Maybe (Entity User, Entity Password))
getUserPassword email = fmap listToMaybe $
  select $
  from $ \(user `InnerJoin` pass) -> do
  on (user ^. UserId ==. pass ^. PasswordUser)
  where_ (user ^. UserEmail ==. val email)
  return (user, pass)

getUserEntity :: Text -> DB (Maybe (Entity User))
getUserEntity email = fmap listToMaybe $
  select $
  from $ \user -> do
  where_ (user ^. UserEmail ==. val email)
  return user

createUser :: Text -> Text -> DB (Entity User)
createUser email pass = do
  let newUser = User email
  userId <- insert $ newUser
  hash <- liftIO $ hashPassword pass
  _ <- insert $ Password hash userId
  return (Entity userId newUser)

createAdmin :: Key User -> DB (Entity Admin)
createAdmin userKey = do
  let newAdmin = Admin userKey
  adminKey <- insert $ newAdmin
  return (Entity adminKey newAdmin)

createCompany :: Text -> Text -> Text -> Text -> Text -> DB (Entity Company)
createCompany title website description image ticker = do
  now <- liftIO $ getCurrentTime
  let newCompany = Company title (Just website) (Just description) (Just image) ticker now
  companyId <- insert $ newCompany
  return (Entity companyId newCompany)

allCompanies :: DB [Entity Company]
allCompanies = do
  companies <- select $
    from $ \company -> do
    return company
  return companies

getCompanyCount :: IO (Database.Esqueleto.Value Int)
getCompanyCount = do
  (companies:_) :: [Database.Esqueleto.Value Int] <-
    runDBA $
    select $
    from $ \(_ :: SqlExpr (Entity Company)) -> do
      return $ countRows
  return companies

getUserCount :: IO (Database.Esqueleto.Value Int)
getUserCount = do
  (users:_) :: [Database.Esqueleto.Value Int] <-
    runDBA $
    select $
    from $ \(_ :: SqlExpr (Entity User)) -> do
      return $ countRows
  return users

getArticleCount :: IO (Database.Esqueleto.Value Int)
getArticleCount = do
  (articles:_) :: [Database.Esqueleto.Value Int] <-
    runDBA $
    select $
    from $ \(_ :: SqlExpr (Entity Story)) -> do
      return $ countRows
  return articles

getHistoryCount :: IO (Database.Esqueleto.Value Int)
getHistoryCount = do
  (history:_) :: [Database.Esqueleto.Value Int] <-
    runDBA $
    select $
    from $ \(_ :: SqlExpr (Entity Historical)) -> do
      return $ countRows
  return history

deleteAllCompanies :: DB Int64
deleteAllCompanies =
  Database.Esqueleto.deleteCount $ from $ \(_ :: SqlExpr (Entity Company)) -> return ()

deleteAdminUsers :: Text -> DB ()
deleteAdminUsers email = do
  mUser <- selectFirst [UserEmail P.==. email] []
  case mUser of
    Nothing -> return ()
    Just u -> do
      Database.Esqueleto.delete $
         from $ \p  -> do
         where_ (p ^. AdminAccount  ==. val (entityKey u))
         return ()

deleteAdminPasswords :: Text -> DB ()
deleteAdminPasswords email = do
  mUser <- selectFirst [UserEmail P.==. email] []
  case mUser of
    Nothing -> return ()
    Just u -> do
      Database.Esqueleto.delete $
        from $ \p -> do
        where_ (p ^. PasswordUser ==. val (entityKey u))
        return ()

countUsersByEmail :: Text -> DB Int
countUsersByEmail email = do
  (cnt:_) :: [Database.Esqueleto.Value Int] <-
    select $
     from $ \u -> do
     where_ (u ^. UserEmail ==. val email)
     return $ countRows
  return $ unValue cnt

deleteUserAdmins :: Text -> DB Int64
deleteUserAdmins email =
  Database.Esqueleto.deleteCount $
  from $ \u -> do
    where_ (u ^. UserEmail ==. val email)


dumpMigration :: DB ()
dumpMigration = printMigration migrateAll

runMigrations :: DB ()
runMigrations = runMigration migrateAll

runDBSqlite :: DB a -> IO a
runDBSqlite = runSqlite "investments-info.sqlite3"

devConn :: ConnectionString
devConn =
 "dbname=investments_info host=localhost user=ii password=R3gc)^tAxiMqNosX@Aeve(xP port=5432"

runDBA :: DB a -> IO a
runDBA a =
  runNoLoggingT $
    withPostgresqlPool devConn 10
      $ \pool -> liftIO $ runSqlPersistMPool a pool

runDevDBV :: DB a -> IO a
runDevDBV a =
  runStdoutLoggingT $
    withPostgresqlPool devConn 10
      $ \pool -> liftIO $ runSqlPersistMPool a pool
