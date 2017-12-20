module Handler.NewsletterView where

import Import
import qualified Text.HTML.Fscraper as F
import Text.Hamlet (hamletFile)

getNewsletterViewR :: Handler Html
getNewsletterViewR = do
  now <- liftIO getCurrentTime
  allStories <- runDB $ selectList [] [Desc StoryCreated, LimitTo 6]
  newsletterLayout $ do
      setTitle "Investments info"
      toWidget [whamlet|
<section id="intro" class="main">
    <div class="spotlight">
        <div class="content">
            <header class="major">
                <h2>Newsletter Issue 0</h2>
                <p .pull-right>#{show now}
                <ul class="features">
                  $forall Entity _ Story{..} <- allStories
                    <li>
                        <a href=#{(pack F.reutersUrl) <> storyLink} target=_blank> #{storyTitle}
                        <p>
                            $maybe img <- storyImage
                                   <a href=#{(pack F.reutersUrl) <> storyLink} target=_blank><img src=#{img} width=100 />
                        <p>
                            $maybe content <- storyContent
                                   <div>
                                      <p>#{show storyCreated}
                                      <p>#{content}
|]

newsletterLayout :: Widget -> Handler Html
newsletterLayout widget = do
    master <- getYesod
    pc <- widgetToPageContent $ do
      $(widgetFile "newsletter-layout")
    withUrlRenderer $(hamletFile "templates/layout/newsletter-layout-wrapper.hamlet")
