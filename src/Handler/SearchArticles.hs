{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Handler.SearchArticles where

import           Database.Esqueleto as E
import           Import

postSearchArticlesR :: Handler Import.Value
postSearchArticlesR = do
  postData <- lookupPostParam "sstr" -- :: Handler ArticleSearchString
  let searchStr = "%" <> (fromMaybe "###############" postData) <> "%"
  articles <-
    runDB $
    select $
    E.from $ \a -> do E.where_ (a ^. StoryTitle `E.ilike` (E.val searchStr))
                      E.limit 10
                      E.orderBy [desc (a ^. StoryCreated)]
                      return a
  return $ object ["result" .= articles]
