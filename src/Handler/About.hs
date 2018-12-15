{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Handler.About where

import Import
import Database.Esqueleto (unValue)

getAboutR :: Handler Html
getAboutR  = do
      companyCount <- lift getCompanyCount
      userCount <- lift getUserCount
      articleCount <- lift getArticleCount
      historyCount <- lift getHistoryCount
      defaultLayout $ do
        setTitle "Finance portal"
        toWidget [whamlet|

<section id="first" class="main">
    <header class="major">
        <p>We are building the investment platform to support people in making better investment decisions.
        <p>The latest invetions in Machine Learning and Artificial Intelligence will power invetments-info.com to:
        <dl class="alt">
            <dd>read, track, filter and consolidate global news</dd>
            <dd>drive investments strategies and predictive models in navigating financial markets fluctuations


<section id="second" class="main special">
    <ul class="statistics">
        <li class="style1">
            <span class="icon fa-code-fork"></span>
            <strong>#{unValue articleCount}</strong> Article number
        <li class="style2">
            <span class="icon fa-folder-open-o"></span>
            <strong>#{unValue companyCount}</strong> Companies listed
        <li class="style4">
            <span class="icon fa-laptop"></span>
            <strong>#{unValue userCount}</strong> Registered users
        <li class="style5">
            <span class="icon fa-database"></span>
            <strong>#{unValue historyCount}</strong> Historical data

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
