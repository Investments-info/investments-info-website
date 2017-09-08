{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import qualified Text.HTML.Fscraper as F
import Database.Esqueleto as E
import Database.Esqueleto.Internal.Language

-- Define our data that will be used for creating the form.
data FileForm = FileForm
  { fileInfo :: FileInfo
  , fileDescription :: Text
  }

getHomeR :: Handler Html
getHomeR = do
  -- firststory <- runDB $ selectFirst [StoryTitle Import.!=. storyTitle ""]
  allStories <- runDB $ selectList [] [Desc StoryCreated]
  (formWidget, formEnctype) <- generateFormPost sampleForm
  let submission = Nothing :: Maybe FileForm
      handlerName = "getHomeR" :: Text
  defaultLayout $ do
    let (commentFormId, commentTextareaId, commentListId) = commentIds
    aDomId <- newIdent
    setTitle "Finance portal"
    $(widgetFile "homepage")


getTopStory :: IO [Handler (Key Story)]
getTopStory = do
  headStory <- F.topStory "olympics-topStory" F.reutersUrl
  case headStory of
    Nothing -> return []
    Just a -> do
      return $ map insertStory $ a


getFeatureStories :: IO [Handler (Key Story)]
getFeatureStories = do
  stories <- F.featureNews "feature" F.reutersUrl
  case stories of
    Nothing -> return []
    Just a -> do
      return $ map insertStory $ a


getSideStories :: IO [Handler (Key Story)]
getSideStories = do
  stories <- F.leftColumnNews "column2" F.reutersUrl
  case stories of
    Nothing -> return []
    Just a -> do
      return $ map insertStory $ a


sampleForm :: Form FileForm
sampleForm =
  renderBootstrap3 BootstrapBasicForm $
  FileForm <$> fileAFormReq "Choose a file" <*>
  areq textField textSettings Nothing
    -- Add attributes like the placeholder and CSS classes.
  where
    textSettings =
      FieldSettings
      { fsLabel = "What's on the file?"
      , fsTooltip = Nothing
      , fsId = Nothing
      , fsName = Nothing
      , fsAttrs =
          [("class", "form-control"), ("placeholder", "File description")]
      }


commentIds :: (Text, Text, Text)
commentIds = ("js-commentForm", "js-createCommentTextarea", "js-commentList")


insertStory :: F.News -> Handler (Key Story)
insertStory news = do
  now <- liftIO getCurrentTime
  let story = Story { storyTitle = pack $ F.newstitle news
        , storyLink = pack $ F.newslink news
        , storyContent = Just (pack $ F.newstext news)
        , storyImage = Just (pack $ F.newsimage news)
        , storyCreated = now
        }

  sid  <- runDB $ insert $ story
  return sid


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
