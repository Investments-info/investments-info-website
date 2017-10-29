{-# LANGUAGE RecordWildCards #-}
module Handler.CompanyDetails where

import Import

getCompanyDetailsR :: CompanyId -> Handler Html
getCompanyDetailsR cid = do
    Company {..} <- runDB $ get404 cid
    defaultLayout $ do
     setTitle "Investments info"
     toWidget [whamlet|
<section id="intro" class="main">
  <div class="container">
    <div class="content">
    <header class="major">
      <h2>Company Details : #{companyTitle}</h2>
      <div style="max-width:800px">
        <ul id="tabs" class="nav nav-tabs" data-tabs="tabs">
          <li class="active">
           <a href="#company-info" data-toggle="tab">Company Info
          <li>
           <a href="#company-historical" data-toggle="tab">Historical Data

        <div id="my-tab-content" class="tab-content">
          <div class="tab-pane active" id="company-info">
            <h5>Company name: #{companyTitle}
            <h5>Company ticker: #{companyTicker}
            $maybe img <- companyImage
                 <p>
                   <img src="#{img}" />
            $maybe web <- companyWebsite
                 <p>website :
                    <a href=#{web}>#{web}

            $maybe desc <- companyDescription
                 <h5>Company Description:
                 <p>#{desc}

          <div class="tab-pane" id="company-historical" >
            <h3>Historical Data
            <div style="max-width:800px;min-height:400px">
               <div class="ct-chart">

|]
     toWidget[lucius|




|]

     toWidget[julius|
    jQuery(document).ready(function ($) {
        $('#tabs').tab();
        $('.nav-tabs').bind('click', function (e){
          if(e.target.innerText == "Historical Data"){
             fetchHistorical();
          }
        });
      });
        function fetchHistorical(){
         $.ajax({
            url: "@{HistoricalR cid}",
            type: "get",
            success: function(data) {
               var data = data;
               var labels = [];
               var open = [];
               var close = [];
               var low = [];
               var high = [];
               var volume = [];
               for(var i = 0; i < data.length;i++){
                  labels.push(data[i].recordDate.substring(0,10));
                  open.push(data[i].recordOpen);
                  close.push(data[i].recordClose);
                  low.push(data[i].recordLow);
                  high.push(data[i].recordHigh);
                  volume.push(data[i].recordVolume);
               }
               new Chartist.Line('.ct-chart', {
                  labels: labels,
                  series: [open, close, high, low,volume]
                }, {
                  low: 0
                });
          },
          error: function(err){
              console.log(err);
            }
          });
      }
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
