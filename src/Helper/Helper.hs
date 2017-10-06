{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Helper.Helper where

import Database.Persist.Sql (SqlBackend, rawSql, unSingle)
import Import
import Prelude (show)



truncateTables :: MonadIO m => ReaderT SqlBackend m [Text]
truncateTables = do
  result <- rawSql "DELETE FROM 'story';" []
  return (fmap unSingle result)

makeHash :: Hashable a => a -> Int
makeHash s = hash s
