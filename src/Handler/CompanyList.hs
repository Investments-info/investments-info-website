{-# OPTIONS_GHC -Wno-unused-matches #-}
module Handler.CompanyList where

import Import
import Helper.Helper as H
import Database.Esqueleto as E

getCompanyListR :: Page -> Handler Html
getCompanyListR currentPage = do
   now <- liftIO getCurrentTime
   entriesCount <-
      runDB $ selectCount $ \story ->  E.where_ (story ^. CompanyCreated E.<=. E.val now)
   let next = H.calculateNextPage entriesCount H.postsByPage currentPage
   let previous = H.calculatePreviousPage entriesCount H.postsByPage currentPage
   let off = (currentPage - 1) * postsByPage
   companies <- runDB $ selectList [] [Asc CompanyTitle, LimitTo H.postsByPage, OffsetBy off]
   defaultLayout $ do
    setTitle "Investments info"
    toWidget [whamlet|
<section class="main special">
  <div class="content">
      <header>
        <h2>Companies</h2>
        <div .col-md-12 style="text-align:left">
          $forall Entity cid Company{..} <- companies
            <div .col-md-6 >
              <div .company-card>
                <h2>#{companyTitle}
                <p>Ticker:
                  <b>#{companyTicker}
                $maybe img <- companyImage
                  <!-- <img src=#{img} width=100 /> -->
                $nothing
                  <!-- <img src=@{StaticR images_defaultimage_gif} width=100 /> -->
                <p>
                  <a .btn .btn-sm .btn-primary .pull-right href=@{CompanyDetailsR cid}>view details
                <br />
        <div .clearfix>
        <hr />
        <div style="text-align:left">
          $maybe previous <- previous
            <a href=@{CompanyListR previous} class="btn btn-primary"><<

          $maybe next <- next
            <a href=@{CompanyListR next} class="btn btn-primary">>>
         <p class="pull-right">Number of companies : #{entriesCount}
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
            url: "@{SearchCompaniesR}",
            type: "post",
            data: {"sstr": searchString},
            success: function(data) {
            console.log(data);
                if(data.result.length > 0){
                   for(var i = 0;i < data.result.length;i++){
                       var item = $('<div class="search-item"><a href="https://investment-info.com/company/'+ data.result[i].id +'" target="_blank" class="search-item" ><img src="'+ data.result[i].image +'" class="search-image" width="90px" />'+ data.result[i].title + '</a><div style="clear:both"></div></div>');
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
