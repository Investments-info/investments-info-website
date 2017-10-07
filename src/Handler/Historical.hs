module Handler.Historical where

import Import
import Helper.YahooHelper as YH

getHistoricalR :: Handler Value
getHistoricalR = do
  let key = toSqlKey 1
  _ <- YH.saveCompanyData key "A"
  return ":::"
