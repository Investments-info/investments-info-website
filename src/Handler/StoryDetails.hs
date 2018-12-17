{-# LANGUAGE QuasiQuotes     #-}

module Handler.StoryDetails where

import qualified Data.Text as T
import           Import
import qualified Text.HTML.Fscraper as F
import           Universum
import           Yesod.Core
import           Yesod.Persist


getStoryDetailsR :: Int -> Handler Html
getStoryDetailsR hashId = do
  s <- runDB $ selectFirst [StoryHashId ==. hashId] []
  defaultLayout $
    toWidget [whamlet|
<section id="content" class="main">
  <span class="image main"><img src="@{StaticR images_pic04_jpg}" alt="" /></span>
  $maybe Entity _ story <-  s
     <h2>#{storyTitle story}</h2>
     <div>
       <p>
         $maybe img <- storyImage story
             <a href=#{(T.pack F.reutersUrl) <> storyLink story} target=_blank><img src=#{img} width=100 />
         $nothing
             <a href=#{(T.pack F.reutersUrl) <> storyLink story} target=_blank><img src=@{StaticR images_defaultimage_gif} width=100 />
         $maybe content <- storyContent story
             <p>#{content}
|]
