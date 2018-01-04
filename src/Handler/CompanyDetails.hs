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
      <br />
      <br />
      <h2>Company Details : #{companyTitle}</h2>
      <div style="max-width:900px">
        <ul id="tabs" class="nav nav-tabs" data-tabs="tabs">
          <li class="active">
           <a href="#company-info" data-toggle="tab">Company Info
          <li>
           <a href="#company-historical" data-toggle="tab">Historical Data

        <div id="my-tab-content" class="tab-content">
          <div class="tab-pane active" id="company-info">
            <br />
            <br />
            <h5>Company name:<b> #{companyTitle} </b>
            <h5>Company ticker:<b> #{companyTicker} </b>
            $maybe gicss <- companyGicssector
              <h5>GICS Sector:<b> #{gicss} </b>
            $maybe gicssub <- companyGicssubindustry
              <h5>GICS Sub Industry:<b> #{gicssub} </b>
            $maybe img <- companyImage
              <p>
                 <img src="#{img}" />
            $maybe web <- companyWebsite
              <p>website :
                 <a href=#{web} .desc> #{web}
            $maybe desc <- companyDescription
              <h5><b>Company Description:</b>
              <p .desc>#{desc}

          <div class="tab-pane" id="company-historical" >
            <h3>Historical Data
               <div id="message">
               <div id="chart_div">
               <div id="chart_volume_div">

      <p>
        <a .btn .btn-sm .btn-primary  href=@{CompanyListR 1} >Back to company list
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
               if(data.length == 0){
                 $("#message").text("We currently don't have any historical data for this company. Please check again in few hours.")
                 return;
               }
               var chartData = [];
               var volumeData = [];

               for(var i = 0; i < data.length;i++){
                  var row = [];
                  var rowVolume = [];
                  row.push(new Date(data[i].recordDate.substring(0,10)));
                  row.push(data[i].recordOpen);
                  row.push(data[i].recordClose);
                  row.push(data[i].recordHigh);
                  row.push(data[i].recordLow);
                  rowVolume.push(new Date(data[i].recordDate.substring(0,10)));
                  rowVolume.push(data[i].recordVolume);
                  chartData.push(row);
                  volumeData.push(rowVolume);
               }

google.charts.load('current', {'packages':['line', 'corechart']});
google.charts.setOnLoadCallback(drawChart);
google.charts.setOnLoadCallback(drawVolumeChart);

    function drawChart() {

      var chartDiv = document.getElementById('chart_div');

      var data = new google.visualization.DataTable();
      data.addColumn('date', 'Date');
      data.addColumn('number', "Open");
      data.addColumn('number', "Close");
      data.addColumn('number', "High");
      data.addColumn('number', "Low");
      data.addRows(chartData);

      var materialOptions = {
        chart: {
          title: 'Market Data'
        },
        width: 800,
        height: 500,
        series: {
          0: {axis: 'Date'},
          1: {axis: 'USD'}
        }
      };


      function drawMaterialChart() {
        var materialChart = new google.charts.Line(chartDiv);
        materialChart.draw(data, materialOptions);
      }

      drawMaterialChart();

    }



    function drawVolumeChart() {

      var volumeDiv = document.getElementById('chart_volume_div');

      var data = new google.visualization.DataTable();
      data.addColumn('date', 'Date');
      data.addColumn('number', "Volume");
      data.addRows(volumeData);

      var materialOptions = {
        chart: {
          title: 'Market Data'
        },
        width: 800,
        height: 500,
        series: {
          0: {axis: 'Date'},
          1: {axis: 'USD'}
        }
      };


      function drawMaterialVolumeChart() {
        var materialChart = new google.charts.Line(volumeDiv);
        materialChart.draw(data, materialOptions);
      }

      drawMaterialVolumeChart();

    }

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
