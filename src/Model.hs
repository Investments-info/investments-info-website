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
module Model
  ( module Import
  , module Model
  ) where

import ClassyPrelude.Yesod hiding ((==.), hash, on)
import Data.Maybe (listToMaybe)
import Database.Esqueleto
import Database.Persist.Sqlite (runSqlite)
import Model.BCrypt as Import
import Model.Instances as Import ()

type ControlIO m = (MonadIO m, MonadBaseControl IO m)

type DBM m a =
  (ControlIO m, MonadThrow m, Monad m) => SqlPersistT m a

type DB a = forall m. DBM m a

type DBVal val =
  ( PersistEntity val
  , PersistEntityBackend val ~ SqlBackend
  , PersistStore (PersistEntityBackend val))

-- fetchThingByField
--   :: (PersistField typ, DBVal val)
--   => EntityField val typ -> typ -> DB (Maybe (Entity val))
-- fetchThingByField field u = selectFirst [field ==. u] []

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

deleteAllCompanies :: DB ()
deleteAllCompanies =
  Database.Esqueleto.delete $ from $ \(_ :: SqlExpr (Entity Company)) -> return ()

deleteAdminUsers :: DB ()
deleteAdminUsers =
  Database.Esqueleto.delete $ from $ \(_ :: SqlExpr (Entity Admin)) -> return ()

deleteUserAdmins :: DB ()
deleteUserAdmins =
  Database.Esqueleto.delete $
  from $ \u -> do
    where_ (u ^. UserEmail ==. val "brutallesale@gmail.com")
    where_ (u ^. UserEmail ==. val "vpleta@gmx.ch")


dumpMigration :: DB ()
dumpMigration = printMigration migrateAll

runMigrations :: DB ()
runMigrations = runMigration migrateAll

runDBA :: DB a -> IO a
runDBA = runSqlite "investments-info.sqlite3"
