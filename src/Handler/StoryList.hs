{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -fno-warn-unused-matches  #-}

module Handler.StoryList where

import qualified Data.Text as T
import           Data.Time (getCurrentTime)
import           Database.Esqueleto as E
import           Helper.Helper as H
import           Import
import qualified Text.HTML.Fscraper as F
import           Universum hiding ((^.))
import           Yesod.Core
import           Yesod.Persist

getStoryListR :: Page -> Handler Html
getStoryListR currentPage = do
  now <- liftIO getCurrentTime
  entriesCount <-
    runDB $ selectCount $ \story ->  E.where_ (story ^. StoryCreated E.<=. E.val now)
  let next = H.calculateNextPage entriesCount H.postsByPage currentPage
  let previous = H.calculatePreviousPage entriesCount H.postsByPage currentPage
  let off = (currentPage - 1) * postsByPage
  allStories <-
    runDB $ selectList [] [Desc StoryCreated, LimitTo H.postsByPage, OffsetBy off]
  defaultLayout $ do
    toWidget [whamlet|
    <section id="content" class="main">
    <h2>Financial news</h2>
      <ul .alt>
      $forall Entity _ Story{..} <- allStories
            <li .list-group-item>
              <div>
                <h4><a href=#{F.buildFullUrl F.reutersUrl storyLink } target=_blank> #{storyTitle}
                <p>
                  $maybe img <- storyImage
                    <a href=#{(T.pack F.reutersUrl) <> storyLink} target=_blank><img src=#{img} width=100 />
                  $nothing
                    <a href=#{(T.pack F.reutersUrl) <> storyLink} target=_blank><img src=@{StaticR images_defaultimage_gif} width=100 />
                  $maybe content <- storyContent
                    <p>#{content}
    <hr />
    $maybe previous <- previous
      <a href=@{StoryListR previous} class="btn btn-primary"><<

    $maybe next <- next
      <a href=@{StoryListR next} class="btn btn-primary">>>
    <p class="pull-right">Number of articles : #{entriesCount}

 |]
    toWidget [julius|
$(document).ready(function(){
   $('#nav .hidable').each(function(key, item){
      var hr = $(item).attr('href').replace(/#/g,"");
      if(hr){
       var x = document.getElementById("#" + hr);
       if(!x) $(item).hide();
      }
   });
 });
|]
