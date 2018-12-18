{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Handler.NewsletterManager where

import Helper.Helper (getAwsKey)
import II.Newsletter
import Import
import MailchimpSimple as MC

listName :: String
listName = "investments-info"

getNewsletterManagerR :: Handler Html
getNewsletterManagerR = do
  (signupFormWidget, _) <- generateFormPost signupForm
  renderSignup signupFormWidget

postNewsletterManagerR :: Handler Html
postNewsletterManagerR = do
  ((result, widget), _) <- runFormPost signupForm
  awsAk <- getAwsKey "awsSesAccessKey"
  awsSk <- getAwsKey "awsSesSecretKey"
  case result of
    FormSuccess email -> do
      maybeUser <- runDB (getUserForNewsletter email)
      case maybeUser of
        Nothing -> do
          mailchimpKey <- getAwsKey "mailchimp-api-key"
          (Entity dbUserKey _) <-
            runDB $ createUserForNewsletter email "dummy-pass" (Just 1)
          _ <-
            liftIO $
            MC.addSubscriber
              (unpack mailchimpKey)
              listName
              (unpack email)
              "newsletter-user"
              "subscribed"
          _ <- liftIO $ verifyEmail (encodeUtf8 awsAk) (encodeUtf8 awsSk) email
          setUserSession dbUserKey True
          setMessage
            "You have signed-up for our newsletter! Expect it in your inbox once a week !"
          redirect ProfileR
        Just (Entity dbUKey dbUser) ->
          case userNewsletter dbUser of
            Just 1 -> do
              setMessage "You are already signed-up for our newsletter!"
              redirect HomeR
            _ -> do
              dbUserKeyU <- runDB $ setUserForNewsletter (Just 1) dbUKey
              mailchimpKey <- getAwsKey "mailchimp-api-key"

              _ <-
                liftIO $
                MC.addSubscriber
                  (unpack mailchimpKey)
                  listName
                  (unpack email)
                  "newsletter-user"
                  "subscribed"
              _ <-
                liftIO $ verifyEmail (encodeUtf8 awsAk) (encodeUtf8 awsSk) email
                -- verifyEmail ("bs" :: ByteString) ("bs" :: ByteString) email
              setMessage
                "You have signed-up for our newsletter! Expect it in your inbox once a week !"
              setUserSession dbUserKeyU True
              redirect ProfileR
    _ -> renderSignup widget

signupForm :: Form Text
signupForm =
  renderDivs $ areq textField (named "email" (placeheld "Email")) Nothing

renderSignup :: Widget -> Handler Html
renderSignup widget =
  baseLayout "Signup" Nothing [whamlet|
<section id="content" class="main">
    <div>
      <div .col-md-6>
        <h3> Newsletter
        <form method="POST" action="@{NewsletterManagerR}">
          ^{widget}
          <input .button type="submit" value="Submit">
     <div .clearfix>
|]

redirectIfLoggedIn
  :: (RedirectUrl App r)
  => r -> Handler ()
redirectIfLoggedIn r = do
  maybeUser <- getUser
  case maybeUser of
    Nothing -> pass
    (Just _) -> redirect r
