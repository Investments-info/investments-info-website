{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Helper.YahooDB where

import Import
import qualified Database.Esqueleto as E


insertIfNotSaved :: Historical -> HandlerT App IO String
insertIfNotSaved Historical{..} = do
  (insertedRecords:_) :: [E.Value Int] <-
      runDB $
      E.select $
      E.from $
        \(h :: E.SqlExpr (Entity Historical)) -> do
          E.where_ $ (h E.^. HistoricalRecordDate E.==. E.val historicalRecordDate)
          E.where_ $ (h E.^. HistoricalTicker E.==. E.val historicalTicker)
          return $ E.countRows
  case insertedRecords of
      E.Value 0 -> return "inserted"
           -- runDB $ insert hrec
      _ -> return "already there"
