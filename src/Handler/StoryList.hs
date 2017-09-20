{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Handler.StoryList where

import Import
import Database.Esqueleto as E
import Database.Esqueleto.Internal.Language
import qualified Text.HTML.Fscraper as F

postsByPage :: Int
postsByPage = 10

selectCount
  :: (BaseBackend backend ~ SqlBackend,
      Database.Esqueleto.Internal.Language.From
        SqlQuery SqlExpr SqlBackend t,
      MonadIO m, Num a, IsPersistBackend backend,
      PersistQueryRead backend, PersistUniqueRead backend,
      PersistField a) =>
     (t -> SqlQuery a1) -> ReaderT backend m a
selectCount q = do
  res <- select $ from (\x -> q x >> return countRows)
  return $ fromMaybe 0 $ (\(Value a) -> a) <$> headMay res

calculatePreviousPage :: Int -> Int -> Page -> Maybe Int
calculatePreviousPage entries pageSize currentPage =
  if n <= entries && n > 0
    then Just n
    else Nothing
  where
    n = (pageSize * (currentPage - 1)) `div` pageSize

calculateNextPage :: Int -> Int -> Int -> Maybe Int
calculateNextPage entries pageSize currentPage =
  if n <= ((entries `div` pageSize) + 1) && n > 0
    then Just n
    else Nothing
  where
    n = (pageSize * (currentPage + 1)) `div` pageSize

getStoryListR :: Page -> Handler Html
getStoryListR currentPage = do
    now <- liftIO getCurrentTime
    entriesCount <-
      runDB $ selectCount $ \story ->  E.where_ (story ^. StoryCreated E.<=. E.val now)
    let next = calculateNextPage entriesCount postsByPage currentPage
    let previous = calculatePreviousPage entriesCount postsByPage currentPage
    let off = (currentPage - 1) * postsByPage

    allStories <- runDB $ selectList [] [Desc StoryCreated, LimitTo postsByPage, OffsetBy off]
    defaultLayout $ do
        toWidget [whamlet|
<div .container>
    <div .bs-docs-section>
        <div .row>
            <div .col-lg-12>
                <div .page-header>
                    <h2 #start>Financial news
                    <ul .list-group>
                        $forall Entity _ news <- allStories
                            <li .list-group-item>
                                <div>
                                    <h4><a href=#{(pack F.reutersUrl) <> storyLink news} target=_blank> #{storyTitle news}
                                    <p>
                                        $maybe img <- storyImage news
                                            <a href=#{(pack F.reutersUrl) <> storyLink news} target=_blank><img src=#{img} width=100 />
                                        $maybe content <- storyContent news
                                            <p>#{content}
                    <hr />
                    $maybe previous <- previous
                        <a href=@{StoryListR previous} class="btn btn-primary"><<

                    $maybe next <- next
                        <a href=@{StoryListR next} class="btn btn-primary">>>
                    <p class="pull-right">Number of articles : #{entriesCount}

|]
