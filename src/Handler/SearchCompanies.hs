{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.SearchCompanies where

import           Data.Aeson
import           Database.Esqueleto as E
import           Import
import           Universum hiding ((^.))

postSearchCompaniesR:: Handler Data.Aeson.Value
postSearchCompaniesR = do
  postData <- lookupPostParam "sstr"
  let searchStr = "%" <> fromMaybe "###############" postData <> "%"
  companies <-
    runDB $
    select $
    E.from $ \a -> do E.where_ (a ^. CompanyTitle `E.ilike` E.val searchStr)
                      E.limit 10
                      return a
  print companies
  return $ object ["result" .= companies]
