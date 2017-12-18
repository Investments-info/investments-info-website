{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}
module Handler.Home where

import Import
import qualified Text.HTML.Fscraper as F
import Data.Time.Clock (diffUTCTime)
import Helper.Helper  as H
import qualified Data.ByteString.Lazy as L


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
      allStories <- runDB $ selectList [] [Desc StoryCreated, LimitTo 6]
      defaultLayout $ do
        setTitle "Finance portal"
        $(widgetFile "homepage")
    Just fs -> do
      let tdiff = diffUTCTime now (storyCreated $ entityVal fs)
      if (tdiff > 3600)
        then do
          _ <- mapM checkStorySaved allS
          return ()
        else return ()
      allStories <- runDB $ selectList [] [Desc StoryCreated, LimitTo 6]
      defaultLayout $ do
        setTitle "Investments info"
        toWidget [whamlet|
<section id="intro" class="main">
    <div class="spotlight">
        <div class="content">
            <header class="major">
                <a href=@{NewsletterManagerR}>Newsletter Signup
                <h2>Financial News</h2>
                <p>We scrape most visited financial portals and display the agregated news to our readers</p>
                <ul class="features">
                  $forall Entity _ Story{..} <- allStories
                    <li>
                        <a href=#{(pack F.reutersUrl) <> storyLink} target=_blank> #{storyTitle}
                        <p>
                            $maybe img <- storyImage
                                   <a href=#{(pack F.reutersUrl) <> storyLink} target=_blank><img src=#{img} width=100 />
                            $nothing
                                   <a href=#{(pack F.reutersUrl) <> storyLink} target=_blank><img src=@{StaticR images_defaultimage_gif} width=100 />

            <ul class="actions">
                  <li><a href="@{StoryListR 1}" class="button">All articles</a></li>

|]

        toWidget [julius|
 $(document).ready(function(){
   var searchString = "";
   $("#article-finder").on('keyup', function(e){
       searchString = $(this).val();
        $("#search-results").css({'display':'none'});
        $("#search-results").empty();
       if(searchString.length > 1){
        $("#search-results").css({'display':'block'});
         $.ajax({
            url: "@{SearchArticlesR}",
            type: "post",
            data: {"sstr": searchString},
            success: function(data) {
                if(data.result.length > 0){
                   for(var i = 0;i < data.result.length;i++){
                       var item = $('<div class="search-item"><a href="http://www.reuters.com/finance/markets' + data.result[i].link +'" target="_blank" class="search-item" ><img src="'+ data.result[i].image +'" class="search-image" width="90px" />'+ data.result[i].title + '</a><div style="clear:both"></div></div>');
                       item.appendTo("#search-results");
                   }
                   $('#search-results img').each(function(index,element){
                     var $el = $(this)
                     if($el.attr('src') == '' || $el.attr('src') == 'null') $el.attr('src','static/images/defaultimage.gif');
                   });
                }else{
                     $("#search-results").css({'display':'none'});
                }
            }
         });
       }
  });
 });
|]

        toWidget [julius|
$(document).ready(function(){
   $('#nav .hidable').each(function(key, item){
      var hr = $(item).attr('href').replace(/#/g,"");
      if(hr){
       var x = document.getElementById("#" + hr);
       if(!x) $(item).parent().remove();
      }
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
