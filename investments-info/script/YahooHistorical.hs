module YahooHistorical where

import Import

api_url :: [Char]
api_url = "https://query.yahooapis.com/v1/public/yql?q="
    <> query <>
    "&format=json&diagnostics=true&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys&callback="

query :: [Char]
query = "select%20*%20from%20yahoo.finance.historicaldata%20where%20symbol%20%3D%20%22YHOO%22%20and%20startDate%20%3D%20%222009-09-11%22%20and%20endDate%20%3D%20%222010-03-10%22"
