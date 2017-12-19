module Handler.NewsletterManager where

import Import
import MailchimpSimple as MC

listName :: String
listName = "investments-info"

apiKey :: String
apiKey = "ab4685034f82cdd3c97286e4839b7cee-us17"

getNewsletterManagerR :: Handler Html
getNewsletterManagerR = do
  -- redirectIfLoggedIn HomeR
  (signupFormWidget, _) <- generateFormPost signupForm
  renderSignup signupFormWidget

postNewsletterManagerR :: Handler Html
postNewsletterManagerR = do
  -- redirectIfLoggedIn HomeR
  ((result, widget), _) <- runFormPost signupForm
  case result of
    FormSuccess email -> do
      maybeUser <- runDB (getUserForNewsletter email)
      case maybeUser of
        Nothing -> do
            (Entity dbUserKey _) <- runDB $ createUserForNewsletter email "dummy-pass" (Just 1)
            _ <- liftIO $ MC.addSubscriber apiKey listName (unpack email) "newsletter-user" "subscribed"
            setUserSession dbUserKey True
            setMessage "You have signed-up for our newsletter! Expect it in your inbox once a week !"
            redirect HomeR
        Just (Entity dbUKey dbUser) -> do
            case userNewsletter dbUser of
                Just 1 -> do
                    setMessage "You are already signed-up for our newsletter!"
                    redirect HomeR
                _ -> do
                    dbUserKeyU <- runDB $ setUserForNewsletter (Just 1) dbUKey
                    _ <- liftIO $ MC.addSubscriber apiKey listName (unpack email) "newsletter-user" "subscribed"
                    setMessage "You have signed-up for our newsletter! Expect it in your inbox once a week !"
                    setUserSession dbUserKeyU True
                    redirect HomeR

    _ -> renderSignup widget

signupForm :: Form Text
signupForm =
  renderDivs $
      areq textField (named "email" (placeheld "Email")) Nothing

renderSignup :: Widget -> Handler Html
renderSignup widget = do
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

redirectIfLoggedIn :: (RedirectUrl App r) => r -> Handler ()
redirectIfLoggedIn r = do
  maybeUser <- getUser
  case maybeUser of
    Nothing -> return ()
    (Just _) -> redirect r
