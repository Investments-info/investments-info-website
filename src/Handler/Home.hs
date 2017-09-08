{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Yesod.Form.Bootstrap3 (BootstrapFormLayout (..), renderBootstrap3)
import Text.Julius (RawJS (..))
import qualified Text.HTML.Fscraper as F

-- Define our data that will be used for creating the form.
data FileForm = FileForm
  { fileInfo :: FileInfo
  , fileDescription :: Text
  }

getHomeR :: Handler Html
getHomeR = do
  topn <- liftIO getTopStory
  siden <- liftIO getSideStories
  featuren <- liftIO getFeatureStories
  (formWidget, formEnctype) <- generateFormPost sampleForm
  let submission = Nothing :: Maybe FileForm
      handlerName = "getHomeR" :: Text
  defaultLayout $ do
    let (commentFormId, commentTextareaId, commentListId) = commentIds
    aDomId <- newIdent
    setTitle "Finance portal"
    $(widgetFile "homepage")

getTopStory :: IO [F.News]
getTopStory = do
  headStory <- F.topStory "olympics-topStory" F.reutersUrl
  case headStory of
    Nothing -> return []
    Just a -> return a

getFeatureStories :: IO [F.News]
getFeatureStories = do
  stories <- F.featureNews "feature" F.reutersUrl
  case stories of
    Nothing -> return []
    Just a -> return a

getSideStories :: IO [F.News]
getSideStories = do
  stories <- F.leftColumnNews "column2" F.reutersUrl
  case stories of
    Nothing -> return []
    Just a -> return a


postHomeR :: Handler Html
postHomeR = do
  topn <- liftIO getTopStory
  siden <- liftIO getSideStories
  featuren <- liftIO getFeatureStories
  ((result, formWidget), formEnctype) <- runFormPost sampleForm
  let handlerName = "postHomeR" :: Text
      submission =
        case result of
          FormSuccess res -> Just res
          _ -> Nothing
  defaultLayout $ do
    let (commentFormId, commentTextareaId, commentListId) = commentIds
    aDomId <- newIdent
    setTitle "Welcome To Yesod!"
    $(widgetFile "homepage")

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

insertStory news = do
  now <- liftIO getCurrentTime
  let story = Story { storyTitle = pack $ F.newstitle news
        , storyLink = pack $ F.newslink news
        , storyContent = Just (pack $ F.newstext news)
        , storyImage = Just (pack $ F.newsimage news)
        , storyCreated = now
        }

  _ <- runDB insert $ story
  redirect HomeR
