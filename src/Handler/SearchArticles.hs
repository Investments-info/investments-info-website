{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Handler.SearchArticles where

import Import
import Database.Esqueleto as E

postSearchArticlesR :: Handler Import.Value
postSearchArticlesR = do
  postData <- lookupPostParam "sstr" -- :: Handler ArticleSearchString
  let searchStr = "%" <> (fromMaybe "" postData) <> "%"
  articles <-
    runDB $
    select $
    E.from $ \a -> do E.where_ (a ^. StoryTitle `E.like` (E.val searchStr))
                      return a
  return $ object ["result" .= articles]
