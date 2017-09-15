{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Helper.Helper where

import Database.Persist.Sql  (SqlBackend, rawSql, unSingle)
import Data.Text
import Data.Hashable
import Data.Map.Strict as M
import Import


truncateTables :: MonadIO m => ReaderT SqlBackend m [Text]
truncateTables = do
    result <- rawSql "DELETE FROM 'story';" []
    return (fmap unSingle result)

makeHash :: Hashable a => a -> Int
makeHash s = hash s

checkStorySaved :: Story -> HandlerT App IO Bool
checkStorySaved story = do
    insertedStory <- runDB $ selectFirst [StoryHashId ==. storyHashId story] []
    case insertedStory of
        Nothing -> return False
        Just s -> return True
