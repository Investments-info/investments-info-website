{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Handler.About where

import Import


getAboutR :: Handler Html
getAboutR  = do
      defaultLayout $ do
        setTitle "Finance portal"
        toWidget [whamlet|

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
