{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
module Model
  ( module Import
  , module Model
  ) where

import Database.Persist.Quasi
import ClassyPrelude.Yesod hiding ((==.), hash, on)
import Control.Monad.Logger hiding (LoggingT, runLoggingT)
import Data.Maybe (listToMaybe)
import Database.Esqueleto
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

fetchThingByField
  :: (PersistField typ, DBVal val)
  => EntityField val typ -> typ -> DB (Maybe (Entity val))
fetchThingByField field u = undefined -- selectFirst [field ==. u] []

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql=users
    email Text
    UniqueUserEmail email
    deriving Typeable
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
|]

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

dumpMigration :: DB ()
dumpMigration = printMigration migrateAll

runMigrations :: DB ()
runMigrations = runMigration migrateAll

-- devConn :: ConnectionString
-- devConn =
--   "dbname=cards-with-comrades-dev host=localhost user=postgres password=password port=5432"

-- runDevDB :: DB a -> IO a
-- runDevDB a =
--   runNoLoggingT $
--     withPostgresqlPool devConn 3
--       $ \pool -> liftIO $ runSqlPersistMPool a pool

-- runDevDBV :: DB a -> IO a
-- runDevDBV a =
--   runStdoutLoggingT $
--     withPostgresqlPool devConn 3
--       $ \pool -> liftIO $ runSqlPersistMPool a pool
