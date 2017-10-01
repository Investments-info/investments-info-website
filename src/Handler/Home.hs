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
import Control.Exception as X hiding (Handler)
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
      allStories <- runDB $ selectList [] [Desc StoryCreated, LimitTo 5]
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

        toWidget [whamlet|
<section id="intro" class="main">
    <div class="spotlight">
        <div class="content">
            <header class="major">
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

<section id="first" class="main special">
    <header class="major">
        <h2>Machine learning and statistical models</h2>
        <ul class="features">
            <li>
                <span class="icon major style1 fa-code"></span>
                <h3>Strong types</h3>
                <p>All code is written in highly expressive and strictly typed language called Haskell.</p>
            <li>
                <span class="icon major style3 fa-database"></span>
                <h3>Learning algorythms</h3>
                <p>We provide ready set of algorithms usable in extracting just the data you need.</p>
            <li>
                <span class="icon major style5 fa-cloud-download"></span>
                <h3>Financial graphs</h3>
                <p>All market fluctuations can be visually tracked in the graphs providing custom options</p>
        <footer class="major">
            <ul class="actions">
                <li><a href="generic.html" class="button">Learn More</a></li>

<section id="second" class="main special">
    <header class="major">
        <h2>Data in your hands</h2>
        <p>These are up to date numbers showing our system health. Feel free to join and explore our services.</p>
    <ul class="statistics">
        <li class="style1">
            <span class="icon fa-code-fork"></span>
            <strong>5,120</strong> Article number
        <li class="style2">
            <span class="icon fa-folder-open-o"></span>
            <strong>8,192</strong> Companies listed
        <li class="style3">
            <span class="icon fa-signal"></span>
            <strong>2,048</strong> Nodes
        <li class="style4">
            <span class="icon fa-laptop"></span>
            <strong>4,096</strong> Registered users
        <li class="style5">
            <span class="icon fa-database"></span>
            <strong>1.889,024</strong> Database records containing financial data
    <p class="content">Nam elementum nisl et mi a commodo porttitor. Morbi sit amet nisl eu arcu faucibus hendrerit vel a risus. Nam a orci mi, elementum ac arcu sit amet, fermentum pellentesque et purus. Integer maximus varius lorem, sed convallis diam accumsan sed. Etiam porttitor placerat sapien, sed eleifend a enim pulvinar faucibus semper quis ut arcu. Ut non nisl a mollis est efficitur vestibulum. Integer eget purus nec nulla mattis et accumsan ut magna libero. Morbi auctor iaculis porttitor. Sed ut magna ac risus et hendrerit scelerisque. Praesent eleifend lacus in lectus aliquam porta. Cras eu ornare dui curabitur lacinia.</p>
    <footer class="major">
        <ul class="actions">
            <li><a href="generic.html" class="button">Learn More</a></li>

<!-- Get Started -->
<section id="cta" class="main special">
    <header class="major">
        <h2>Congue imperdiet</h2>
        <p>Donec imperdiet consequat consequat. Suspendisse feugiat congue<br />
        posuere. Nulla massa urna, fermentum eget quam aliquet.</p>
    <footer class="major">
        <ul class="actions">
            <li><a href="" class="button special">Get Started</a></li>
            <li><a href="" class="button">Learn More</a></li>
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
