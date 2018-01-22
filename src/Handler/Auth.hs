module Handler.Auth where

import Import
import Text.Email.Validate (isValid)

loginForm :: Form (Text, Text)
loginForm =
  renderDivs $
  (,) <$> areq textField (named "email" (placeheld "Email")) Nothing
      <*> areq passwordField (named "password" (placeheld "Password")) Nothing

redirectIfLoggedIn :: (RedirectUrl App r) => r -> Handler ()
redirectIfLoggedIn r = do
  maybeUser <- getUser
  case maybeUser of
    Nothing -> return ()
    (Just _) -> redirect r

renderLogin :: Widget -> Handler Html
renderLogin widget = do
  baseLayout "Login" Nothing [whamlet|
<section id="content" class="main">
 <div class="row">
  <div class="col-md-12 columns">
    <hr>
 <div class="row" id="content">
  <div class="col-md-8 columns">
    <h3>Login to your account!
    <form method="POST" action="@{LoginR}">
      ^{widget}
      <input class="btn btn-success" type="submit" value="Submit">
  <div class="col-md-4">
     <p>
       <a href="@{SignupR}" class="btn btn-default btn-sm pull-right">Signup
|]

getLoginR :: Handler Html
getLoginR = do
  redirectIfLoggedIn HomeR
  (loginFormWidget, _) <- generateFormPost loginForm
  renderLogin loginFormWidget

postLoginR :: Handler Html
postLoginR = do
  redirectIfLoggedIn HomeR
  ((result, widget), _) <- runFormPost loginForm
  case result of
    FormSuccess (email, password) -> do
      maybeUP <- runDB (getUserPassword email)
      if (not (isValid (encodeUtf8 email))) then
          do
          setMessage "That is not a valid email!"
          redirect LoginR
      else
          case maybeUP of
            Nothing ->
              notFound
            (Just ((Entity dbUserKey _), (Entity _ dbPass))) -> do
              let success = passwordMatches (passwordHash dbPass) password
              case success of
                False -> notAuthenticated
                True -> do
                  setUserSession dbUserKey True
                  redirect ProfileR
    _ -> renderLogin widget


signupForm :: Form (Text, Text)
signupForm = loginForm
  -- renderDivs $
  -- (,) <$> areq textField (named "email" (placeheld "Email")) Nothing
  --     <*> areq passwordField (named "password" (placeheld "Password")) Nothing

renderSignup :: Widget -> Handler Html
renderSignup widget = do
  baseLayout "Login" Nothing [whamlet|
<section id="content" class="main">
 <div class="row">
  <div class="col-md-8 columns">
    <hr>
 <div class="row">
  <div class="col-md-8 columns">
    <h3>Signup for an account!
    <form method="POST" action="@{SignupR}">
      ^{widget}
      <input class="btn btn-success" type="submit" value="Submit">
|]

getSignupR :: Handler Html
getSignupR = do
  redirectIfLoggedIn HomeR
  (signupFormWidget, _) <- generateFormPost signupForm
  renderSignup signupFormWidget

postSignupR :: Handler Html
postSignupR = do
  redirectIfLoggedIn HomeR
  ((result, widget), _) <- runFormPost signupForm
  case result of
    FormSuccess (email, password) -> do
      maybeUP <- runDB (getUserEntity email)
      if (not (isValid (encodeUtf8 email))) then
          do
          setMessage "That is not a valid email"
          renderSignup widget
      else
         -- Check to see if a user with this email already exists
         case maybeUP of
           -- If it does, render the form again (?)
           (Just _) -> do
               setMessage "User already exists"
               renderSignup widget
           -- If not, create a user
           Nothing -> do
               (Entity dbUserKey _) <- runDB $ createUser email password
               setUserSession dbUserKey True
               redirect ProfileR
    _ -> renderSignup widget

getSignoutR :: Handler Html
getSignoutR = do
  deleteLoginData
  redirect HomeR
