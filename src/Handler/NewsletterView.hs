{-# OPTIONS_GHC -Wno-unused-matches #-}
module Handler.NewsletterView where

import Import
import qualified Text.HTML.Fscraper as F
import Text.Hamlet (hamletFile)

getNewsletterViewR :: Handler Html
getNewsletterViewR = do
  now <- liftIO getCurrentTime
  allStories <- runDB $ selectList [] [Desc StoryCreated, LimitTo 10]
  newsletterLayout $ do
      setTitle "Investments info"
      toWidget [whamlet|
    <table border="0" cellpadding="0" cellspacing="0" .body>
      <tr>
        <td>&nbsp;
        <td .container>
          <div .content>
            <table .main>
              <tr>
                <td .wrapper>
                  <h2>Investments Info newsletter
              <tr>
                <td .wrapper>
                 $forall Entity _ Story{..} <- allStories
                  <table border="0" cellpadding="0" cellspacing="0">
                    <tr>
                      <td>
                        <p><a href=#{(pack F.reutersUrl) <> storyLink} target=_blank> #{storyTitle}

            <div .footer>
              <table border="0" cellpadding="0" cellspacing="0">
                <tr>
                  <td .content-block>
                    <span .apple-link>Don't like these emails? <a href="http://localhost:3000/profile">Unsubscribe.
                <tr>
                  <td .content-block .powered-by>
                    <a href="https://investments-info.com">investments-info.com

        <td>&nbsp;
|]

newsletterLayout :: Widget -> Handler Html
newsletterLayout widget = do
    master <- getYesod
    pc <- widgetToPageContent $ do
      $(widgetFile "newsletter-layout")
    withUrlRenderer $(hamletFile "templates/layout/newsletter-layout-wrapper.hamlet")
