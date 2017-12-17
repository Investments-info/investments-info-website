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
      maybeUP <- runDB (getUserPassword email)
      case maybeUP of
        Nothing ->
          -- setUserSession dbUserKey True
          notFound
        Just _ -> do
            setMessage "You are already signed-up for out newsletter!"
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
