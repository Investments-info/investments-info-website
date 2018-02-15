{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.SearchCompanies where

import           Database.Esqueleto as E
import           Import

postSearchCompaniesR:: Handler Import.Value
postSearchCompaniesR = do
  postData <- lookupPostParam "sstr"
  let searchStr = "%" <> (fromMaybe "###############" postData) <> "%"
  companies <-
    runDB $
    select $
    E.from $ \a -> do E.where_ (a ^. CompanyTitle `E.ilike` (E.val searchStr))
                      E.limit 10
                      return a
  print companies
  return $ object ["result" .= companies]
