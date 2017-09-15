{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Handler.Home where

import Import
import qualified Text.HTML.Fscraper as F
import Data.Time.Clock (diffUTCTime)
import Helper.Helper  as H


getHomeR :: Handler Html
getHomeR = do
  now <- liftIO getCurrentTime
  topnews <- liftIO getTopStory
  fnews <- liftIO getFeatureStories
  snews <- liftIO getSideStories
  let topstories =  mapM convertImageStory topnews now
      fstories =  mapM convertImageStory fnews now
      sstories =  mapM convertStory snews now
      allS     = topstories <> fstories <> sstories
  filteredStories <- mapM H.checkStorySaved allS
  firststory <- runDB $ selectFirst [] [Desc StoryCreated]
  case firststory of
      Nothing -> do
              _ <- mapM (runDB . insert) allS
              allStories <- runDB $ selectList [] [Desc StoryCreated]
              defaultLayout $ do
                setTitle "Finance portal"
                $(widgetFile "homepage")
      Just fs -> do
          let tdiff = diffUTCTime now  (storyCreated $ entityVal fs)
          if(tdiff > 3600) then
              do
                _ <- runDB  $ H.truncateTables
                _ <- mapM (runDB . insert) allS
                return ()
          else
              return ()
          allStories <- runDB $ selectList [] [Desc StoryCreated]
          defaultLayout $ do
            setTitle "Finance portal"
            $(widgetFile "homepage")


getTopStory :: IO [F.News]
getTopStory = do
  headStory <- F.topStory "olympics-topStory" F.reutersUrl
  case headStory of
    Nothing -> return []
    Just a -> return a


getFeatureStories :: IO [F.News]
getFeatureStories = do
  stories <- F.featureNews "feature" F.reutersUrl
  case stories of
    Nothing -> return []
    Just a -> return a


getSideStories :: IO [F.News]
getSideStories = do
  stories <- F.leftColumnNews "column2" F.reutersUrl
  case stories of
    Nothing -> return []
    Just a -> return a

convertImageStory :: F.News -> UTCTime -> Story
convertImageStory news now =
  Story
  { storyHashId = H.makeHash (F.newstitle news)
  , storyTitle = pack $ F.newstitle news
  , storyLink = pack $ F.newslink news
  , storyContent = Just (pack $ F.newstext news)
  , storyImage = Just (pack $ F.newsimage news)
  , storyCreated = now
  }


convertStory :: F.News -> UTCTime -> Story
convertStory news now =
  Story
  { storyHashId = H.makeHash (F.newstitle news)
  , storyTitle = pack $ F.newstitle news
  , storyLink = pack $ F.newslink news
  , storyContent = Just (pack $ F.newstext news)
  , storyImage = Nothing
  , storyCreated = now
  }
