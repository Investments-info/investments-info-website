module Handler.Historical where

import Import

getHistoricalR :: CompanyId -> Handler Value
getHistoricalR cid = do
    hData <- runDB $ getAllCompanyHistoricalDataById cid
    returnJson hData
