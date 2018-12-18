module Handler.Historical where

import           Import
import           Universum

getHistoricalR :: CompanyId -> Handler Value
getHistoricalR cid = do
    hData <- runDB $ getAllCompanyHistoricalDataById cid
    returnJson hData
