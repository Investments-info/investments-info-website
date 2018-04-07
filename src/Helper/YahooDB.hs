{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}

module Helper.YahooDB where

import qualified Database.Esqueleto as E
import           Import

insertIfNotSaved :: Maybe Historical -> IO (Key Historical)
insertIfNotSaved Nothing = return $ toSqlKey 0
insertIfNotSaved (Just hrec) = do
  (insertedRecords:_) :: [E.Value Int] <-
    runDBA $
    E.select $
    E.from $ \(h :: E.SqlExpr (Entity Historical)) -> do
      E.where_ $ (h E.^. HistoricalRecordDate E.==. E.val (Import.historicalRecordDate hrec))
      E.where_ $ (h E.^. HistoricalTicker E.==. E.val (Import.historicalTicker hrec))
      return $ E.countRows
  case insertedRecords of
    E.Value 0 -> runDBA $ insert hrec
    _         -> return $ toSqlKey 0
