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

import           Data.Time (UTCTime, getCurrentTime)
import           Data.Yaml
import           Database.Esqueleto
import           Database.Esqueleto.Internal.Sql (SqlSelect)
import qualified Database.Persist as P
import           Database.Persist.TH
import           Model.BCrypt as Import
import           Universum hiding (Key, on, (^.))

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

type Title = Text
type Website = Text
type Description = Text
type Image = Text
type Ticker = Text
type Gicssector = Text
type Gicssubindustry = Text

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

selectHead
  :: (SqlSelect a r, MonadIO m)
  => SqlQuery a -> SqlReadT m (Maybe r)
selectHead query = listToMaybe <$> select query

-------------------------------------------------------
-- create
-------------------------------------------------------

createUser :: MonadIO m => Text -> Text -> SqlPersistT m (Entity User)
createUser email password = do
  now <- liftIO getCurrentTime
  let newUser = User email Nothing Nothing Nothing Nothing Nothing Nothing now
  user <- insertBy newUser
  case user of
    Left (Entity userId _) -> insertHashed password userId newUser
    Right userId -> insertHashed password userId newUser

createUserForNewsletter :: MonadIO m => Text -> Text -> Maybe Int -> SqlPersistT m (Entity User)
createUserForNewsletter email password newsletter = do
  now <- liftIO getCurrentTime
  let newUser = User email Nothing Nothing Nothing Nothing Nothing newsletter now
  user <- insertBy newUser
  case user of
    Left (Entity userId _) -> insertHashed password userId newUser
    Right userId  -> insertHashed password userId newUser

insertHashed :: MonadIO m => Text -> Key User -> User -> SqlPersistT m (Entity User)
insertHashed password userId newUser = do
  hash <- liftIO $ hashPassword password
  void $ insertBy $ Password hash userId
  return (Entity userId newUser)

setUserForNewsletter :: MonadIO m => Maybe Int -> UserId -> SqlPersistT m (Key User)
setUserForNewsletter newsletter userId = do
  P.update userId [UserNewsletter P.=. newsletter]
  return userId

createAdmin :: MonadIO m => Key User -> SqlPersistT m (Either (Entity Admin) AdminId)
createAdmin userKey = do
  let newAdmin = Admin userKey
  insertBy newAdmin

createCompany
  :: MonadIO m
  => Title
  -> Website
  -> Description
  -> Image
  -> Ticker
  -> Gicssector
  -> Gicssubindustry
  -> SqlPersistT m (Entity Company)
createCompany title website description image ticker gicssector gicssubindustry = do
  now <- liftIO getCurrentTime
  let newCompany = Company title (Just website) (Just description) (Just image) ticker (Just gicssector) (Just gicssubindustry) now
  companyId <- insert newCompany
  return (Entity companyId newCompany)

---------------------------------------------------------
-- get
---------------------------------------------------------

getUserPassword :: MonadIO m => Text -> SqlPersistT m (Maybe (Entity User, Entity Password))
getUserPassword email = fmap listToMaybe $
  select $
  from $ \(u `InnerJoin` p) -> do
  on (u ^. UserId ==. p ^. PasswordUser)
  where_ (u ^. UserEmail ==. val email)
  return (u, p)

getUserEntity :: MonadIO m => Text -> SqlPersistT m (Maybe (Entity User))
getUserEntity email = fmap listToMaybe $
  select $
  from $ \user -> do
  where_ (user ^. UserEmail ==. val email)
  return user

getUserForNewsletter :: MonadIO m => Text -> SqlPersistT m (Maybe (Entity User))
getUserForNewsletter email = fmap listToMaybe $
  select $
  from $ \user -> do
  where_ (user ^. UserEmail ==. val email)
  -- where_ (user ^. UserNewsletter ==. val (Just True))
  return user

allCompanies :: MonadIO m => SqlPersistT m [Entity Company]
allCompanies =
  select $
    from $ \c ->
    return c

getCompanyById :: MonadIO m => CompanyId -> SqlPersistT m (Maybe(Entity Company))
getCompanyById cid = fmap listToMaybe $
  select $
  from $ \c -> do
  where_ (c ^. CompanyId ==. val cid)
  return c

getAllCompanyHistoricalDataById :: MonadIO m => CompanyId -> SqlPersistT m [Entity Historical]
getAllCompanyHistoricalDataById cid =
  select $
  from $ \c -> do
  where_ (c ^. HistoricalCompanyId ==. val cid)
  where_ (c ^. HistoricalRecordOpen Database.Esqueleto.>. val 1)
  orderBy [desc (c ^. HistoricalRecordDate)]
  limit 1000
  return c

getLatestUniqueStories :: MonadIO m => SqlPersistT m [Entity Story]
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

deleteAllCompanies :: MonadIO m => SqlPersistT m Int64
deleteAllCompanies =
  Database.Esqueleto.deleteCount $ from $ \(_ :: SqlExpr (Entity Company)) -> pass

deleteAdminUsers :: MonadIO m => Text -> SqlPersistT m ()
deleteAdminUsers email = do
  mUser <- selectFirst [UserEmail P.==. email] []
  case mUser of
    Nothing -> pass
    Just u ->
      Database.Esqueleto.delete $
         from $ \p  -> do
         where_ (p ^. AdminAccount  ==. val (entityKey u))
         pass

deleteAdminPasswords :: MonadIO m => Text -> SqlPersistT m ()
deleteAdminPasswords email = do
  mUser <- selectFirst [UserEmail P.==. email] []
  case mUser of
    Nothing -> pass
    Just u ->
      Database.Esqueleto.delete $
        from $ \p -> do
        where_ (p ^. PasswordUser ==. val (entityKey u))
        pass

deleteUserAdmins :: MonadIO m => Text -> SqlPersistT m Int64
deleteUserAdmins email =
  Database.Esqueleto.deleteCount $
  from $ \u -> where_ (u ^. UserEmail ==. val email)

--------------------------------------------------------
-- count
--------------------------------------------------------

countUsersByEmail :: MonadIO m => Text -> SqlPersistT m Int
countUsersByEmail email = do
  (cnt:_) :: [Database.Esqueleto.Value Int] <-
    select .
     from $ \u -> do
     where_ (u ^. UserEmail ==. val email)
     return countRows
  return $ unValue cnt

getCompanyCount :: MonadIO m => SqlPersistT m (Maybe (Database.Esqueleto.Value Int))
getCompanyCount =
  selectHead .  from $ \(_ :: SqlExpr (Entity Company)) ->
    return countRows

getUserCount :: MonadIO m => SqlPersistT m (Maybe (Database.Esqueleto.Value Int))
getUserCount = do
  selectHead .  from $ \(_ :: SqlExpr (Entity User)) ->
    return countRows

getArticleCount :: MonadIO m => SqlPersistT m (Maybe (Database.Esqueleto.Value Int))
getArticleCount =
  selectHead .  from $ \(_ :: SqlExpr (Entity Story)) ->
    return countRows

getHistoryCount :: MonadIO m => SqlPersistT m (Maybe (Database.Esqueleto.Value Int))
getHistoryCount =
  selectHead . from $ \(_ :: SqlExpr (Entity Historical)) ->
    return countRows

--------------------------------------------------------
-- run actions
--------------------------------------------------------

dumpMigration :: MonadIO m => SqlPersistT m ()
dumpMigration = printMigration migrateAll

runMigrations :: MonadIO m => SqlPersistT m ()
runMigrations = runMigration migrateAll

