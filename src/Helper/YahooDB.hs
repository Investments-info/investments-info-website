{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Helper.YahooDB where

import Import
import qualified Database.Esqueleto as E


insertIfNotSaved :: Historical -> Handler Bool
insertIfNotSaved hrec = do
  (insertedRecords:_) :: [E.Value Int] <-
    runDB $
    E.select $
    E.from $ \(h :: E.SqlExpr (Entity Historical)) -> do
      E.where_ $ (h E.^. HistoricalRecordDate E.==. E.val (historicalRecordDate hrec))
      E.where_ $ (h E.^. HistoricalTicker E.==. E.val (historicalTicker hrec))
      return $ E.countRows
  _ <- runDB $ insert hrec
  case insertedRecords of
    E.Value 0 -> do
        _ <- runDB $ insert hrec
        return True
    _ -> return False
