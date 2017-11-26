{-# LANGUAGE RecordWildCards #-}
module Handler.Company where

import Import
import Helper.FormHelper as FH

getCompanyR :: Handler Html
getCompanyR = do
  _ <- requireAdmin
  now <- liftIO getCurrentTime
  (widget, enctype) <- generateFormPost $ FH.newCompanyForm now
  defaultLayout  $(widgetFile "company/newcompany")

postCompanyR :: Handler Html
postCompanyR = do
    now <- liftIO getCurrentTime
    ((res, _), _) <- runFormPost $ FH.newCompanyForm now
    case res of
      FormSuccess Company{..} -> do
            _ <- runDB $ insert $ Company companyTitle companyWebsite companyImage companyDescription companyTicker companyGicssector companyGicssubindustry now

            setMessage "Company created!"
            redirect CompanyListR
      _ -> do
        setMessage "Company not created"
        redirect CompanyR
