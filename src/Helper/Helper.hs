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
import Import


truncateTables :: MonadIO m => ReaderT SqlBackend m [Text]
truncateTables = do
    result <- rawSql "DELETE FROM 'story';" []
    return (fmap unSingle result)

makeHash :: Hashable a => a -> Int
makeHash s = hash s

checkStorySaved :: [Story] -> HandlerT App IO [Maybe (Entity Story)]
checkStorySaved stories = do
    insertedStories <- mapM (\s -> runDB $ selectFirst [StoryHashId ==. storyHashId s] []) stories
    liftIO $ print insertedStories
    return undefined


filterSavedStory storyList s = undefined
