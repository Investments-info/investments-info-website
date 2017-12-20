{-# OPTIONS_GHC -Wno-unused-matches #-}
module Handler.NewsletterNewUser where

import Import
import Text.Hamlet (hamletFile)

getNewsletterNewUserR :: Handler Html
getNewsletterNewUserR = do
  newsletterLayout $ do
      toWidget [whamlet|
<section id="intro" class="main">
    <div class="spotlight">
        <div class="content">
            <header class="major">
                <h2>Welcome!</h2>
|]

newsletterLayout :: Widget -> Handler Html
newsletterLayout widget = do
    master <- getYesod
    pc <- widgetToPageContent $ do
      $(widgetFile "newsletter-layout")
    withUrlRenderer $(hamletFile "templates/layout/newsletter-layout-wrapper.hamlet")
