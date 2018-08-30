{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Handler.Home where

import Import
import Helper.Helper  as H

getHomeR :: Handler Html
getHomeR  = do
      allStories <- runDB $ selectList [] [Desc StoryCreated, LimitTo 6]
      defaultLayout $ do
        setTitle "Investments info"
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
                        -- <a href=#{(pack H.reutersUrl) <> storyLink} target=_blank> #{storyTitle}
                        <a href=#{ H.buildFullUrl H.reutersUrl storyLink } target=_blank> #{storyTitle}
                        <p>
                            $maybe img <- storyImage
                                   <a href=#{(pack H.reutersUrl) <> storyLink} target=_blank><img src=#{img} width=100 />
                            $nothing
                                   <a href=#{(pack H.reutersUrl) <> storyLink} target=_blank><img src=@{StaticR images_defaultimage_gif} width=100 />

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
