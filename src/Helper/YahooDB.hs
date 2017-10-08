{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGEF lexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Helper.YahooDB where

import Import
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Maybe
import Database.Esqueleto
       (InnerJoin(..), LeftOuterJoin(..), (?.), (^.), from, in_, just, on,
        select, val, valList, where_, Value)
import qualified Database.Esqueleto as E
import Database.Persist
import Database.Persist.Sql
import Database.Persist.Sqlite
import Database.Persist.TH
import Text.Show.Pretty


share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
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

runDBA :: DB a -> IO a
runDBA = runSqlite ":memory:"


insertIfNotSaved :: Historical -> IO (Key Historical)
insertIfNotSaved hrec = do
  (insertedRecords:_) :: [E.Value Int] <-
    runDBA $
    E.select $
    E.from $ \(h :: E.SqlExpr (Entity Historical)) -> do
      E.where_ $ (h E.^. HistoricalRecordDate E.==. E.val (historicalRecordDate hrec))
      E.where_ $ (h E.^. HistoricalTicker E.==. E.val (historicalTicker hrec))
      return $ E.countRows
  case insertedRecords of
    E.Value 0 -> runDBA $ insert hrec
