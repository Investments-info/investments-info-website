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

         <div class="tab-pane" id="company-historical">
            <h3>Historical Data
            <div id="graph"></div>
|]
     toWidget[lucius|
.chart {
    font-family: Arial, sans-serif;
    font-size: 10px;
  }

  .axis path, .axis line {
    fill: none;
    stroke: #000;
    shape-rendering: crispEdges;
  }

  .bar {
    fill: steelblue;
  }
|]
     toWidget[julius|
    jQuery(document).ready(function ($) {
        $('#tabs').tab();
         $.ajax({
            url: "@{HistoricalR cid}",
            type: "get",
            success: function(data) {
               console.log(data);
               var data = data;
var margin = {top: 40, right: 40, bottom: 40, left:40},
    width = 800,
    height = 500;

var x = d3.time.scale()
    .domain([new Date(data[0].recordDate), d3.time.day.offset(new Date(data[data.length - 1].recordDate), 1)])
    .rangeRound([0, width - margin.left - margin.right]);

var y = d3.scale.linear()
    .domain([0, d3.max(data, function(d) { return d.recordOpen; })])
    .range([height - margin.top - margin.bottom, 0]);

var xAxis = d3.svg.axis()
    .scale(x)
    .orient('bottom')
    .ticks(d3.time.days, 1)
    .tickFormat(d3.time.format('%Y %m %d'))
    .tickSize(0)
    .tickPadding(8);

var yAxis = d3.svg.axis()
    .scale(y)
    .orient('left')
    .tickPadding(8);

var svg = d3.select('#graph').append('svg')
    .attr('class', 'chart')
    .attr('width', width)
    .attr('height', height)
  .append('g')
    .attr('transform', 'translate(' + margin.left + ', ' + margin.top + ')');

svg.selectAll('.chart')
    .data(data)
  .enter().append('rect')
    .attr('class', 'bar')
    .attr('x', function(d) { return x(new Date(d.recordDate)); })
    .attr('y', function(d) { return height - margin.top - margin.bottom - (height - margin.top - margin.bottom - y(d.recordOpen)) })
    .attr('width', 10)
    .attr('height', function(d) { return height - margin.top - margin.bottom - y(d.recordOpen) });

svg.append('g')
    .attr('class', 'x axis')
    .attr('transform', 'translate(0, ' + (height - margin.top - margin.bottom) + ')')
    .call(xAxis);

svg.append('g')
  .attr('class', 'y axis')            },
            error: function(err){
              console.log(err);
            }
        });
    });
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
