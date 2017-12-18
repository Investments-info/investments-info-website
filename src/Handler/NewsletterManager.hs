module Handler.NewsletterManager where

import Import

getNewsletterManagerR :: Handler Html
getNewsletterManagerR = do
  redirectIfLoggedIn HomeR
  (signupFormWidget, _) <- generateFormPost signupForm
  renderSignup signupFormWidget

postNewsletterManagerR :: Handler Html
postNewsletterManagerR = do
  redirectIfLoggedIn HomeR
  ((result, widget), _) <- runFormPost signupForm
  case result of
    FormSuccess email -> do
      maybeUser <- runDB (getUserForNewsletter email)
      case maybeUser of
        Nothing -> do
            (Entity dbUserKey _) <- runDB $ createUserForNewsletter email "dummy-pass" (Just True)
            setUserSession dbUserKey True
            setMessage "You have signed-up for our newsletter! Expect it in your inbox (or spam :) ) !"
            redirect HomeR
        Just (Entity dbUserKey dbUser) -> do
            case userNewsletter dbUser of
                Just True -> do
                    setMessage "You are already signed-up for our newsletter!"
                    redirect HomeR
                Nothing -> do
                    dbUserKey <- runDB $ setUserForNewsletter (Just True) dbUserKey
                    setUserSession dbUserKey True
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
