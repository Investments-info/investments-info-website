{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
module Helper.Helper where

import Database.Persist.Sql (SqlBackend, rawSql, unSingle)
import Import

truncateTables :: MonadIO m => ReaderT SqlBackend m [Text]
truncateTables = do
  result <- rawSql "DELETE FROM 'story';" []
  return (fmap unSingle result)

makeHash :: Hashable a => a -> Int
makeHash s = hash s
