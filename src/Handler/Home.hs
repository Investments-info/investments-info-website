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
import LibYahoo (getYahooData)
import Control.Exception as X hiding (Handler)
import qualified Data.ByteString.Lazy as L

data YahooData = YahooData
  { date :: UTCTime
  , open :: Float
  , high :: Float
  , low :: Float
  , close :: Float
  , adjClose :: Float
  , volume :: Int
  } deriving (Show, Eq)

getHomeR :: Handler Html
getHomeR  = do
  now <- liftIO getCurrentTime
  topnews <- getTopStory
  fnews <- getFeatureStories
  snews <- getSideStories
  let topstories = mapM convertImageStory topnews now
      fstories = mapM convertImageStory fnews now
      sstories = mapM convertStory snews now
      allS = topstories <> fstories <> sstories
  firststory <- runDB $ selectFirst [] [Desc StoryCreated]
  case firststory of
    Nothing -> do
      _ <- mapM checkStorySaved allS
      allStories <- runDB $ selectList [] [Desc StoryCreated, LimitTo 5]
      defaultLayout $ do
        setTitle "Finance portal"
        $(widgetFile "homepage")
    Just fs -> do
      let tdiff = diffUTCTime now (storyCreated $ entityVal fs)
      if (tdiff > 3600)
        then do
          _ <- mapM checkStorySaved allS
          graphData <- liftIO (getYahooData "KO")
          print graphData
          return ()
        else return ()
      allStories <- runDB $ selectList [] [Desc StoryCreated, LimitTo 5]
      defaultLayout $ do
        toWidget [whamlet|
<div .masthead>
    <div .container>
        <div .row>
            <h1 .header>
               Investments Info - Finance portal
            <h2>
<div .container>
    <div .bs-docs-section>
        <div .row>
            <div .col-lg-12>
                <div .page-header>
                    <div .pull-right .col-md-3>
                        <input type="text" #article-finder .form-control placeholder="Search articles" value="" />
                        <div id="search-results" style="z-index:5000;border:1px solid gray"></div>
                    <h2 #start>Financial news
                    <ul .list-group>
                        $forall Entity _ news <- allStories
                            <li .list-group-item>
                                <div>
                                    <h4><a href=#{(pack F.reutersUrl) <> storyLink news} target=_blank> #{storyTitle news}
                                    <p>
                                        $maybe img <- storyImage news
                                            <a href=#{(pack F.reutersUrl) <> storyLink news} target=_blank><img src=#{img} width=100 />
                                        $maybe content <- storyContent news
                                            <p>#{content}
                    <a href=@{StoryListR 1} class="btn btn-primary pull-right">All articles
|]
        toWidget [julius|
 $(document).ready(function(){
   var searchString = "";
   $("#article-finder").on('keyup', function(e){
       searchString = $(this).val();
       $("#search0-results").empty();
       $.ajax({
            url: "@{SearchArticlesR}",
            type: "post",
            data: {"sstr": searchString},
            success: function(data) {
            console.log(data);
                if(data.result){
                   for(var i = 0;i < data.result.length;i++){
                      var div = $('<div/>', {
                           id: "story-" + i
                       }).appendTo('#search-results');
                      var l = $('<a/>', {
                          href: data.result[i].link,
                          title: data.result[i].title,
                          rel: 'external',
                          text: data.result[i].title,
                       }).appendTo('#story' + i);
                   }
                }
            }
        });
  });
 });
|]

httpExceptionHandler ::   HttpExceptionContent -> IO (Either String L.ByteString)
httpExceptionHandler (StatusCodeException _ _) = do
          return $ Left "oops"
httpExceptionHandler _ = do
          return $ Left "oops some error"

checkStorySaved :: Story -> HandlerT App IO (Maybe (Entity Story))
checkStorySaved story = do
  insertedStory <- runDB $ selectFirst [StoryHashId ==. storyHashId story] []
  case insertedStory of
    Nothing -> do
      _ <- runDB $ insert story
      return Nothing
    Just s -> return $ Just s

getTopStory :: MonadIO m => m [F.News]
getTopStory = do
  headStory <- liftIO $ F.topStory "olympics-topStory" F.reutersUrl
  case headStory of
    Nothing -> return []
    Just a -> return a


getFeatureStories :: MonadIO m => m [F.News]
getFeatureStories = do
  stories <- liftIO $ F.featureNews "feature" F.reutersUrl
  case stories of
    Nothing -> return []
    Just a -> return a


getSideStories :: MonadIO m => m [F.News]
getSideStories = do
  stories <- liftIO $ F.leftColumnNews "column2" F.reutersUrl
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
