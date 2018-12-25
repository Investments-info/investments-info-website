{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes      #-}

module Handler.Profile where

import           Import
import           Universum

-- move this to helper
listName :: String
listName = "investments-info"

getProfileR :: Handler Html
getProfileR = do
  redirectIfNotLoggedIn HomeR
  user <- getUser
  case user of
    Just (Entity _ u) -> do
      (profileFormWidget, _) <- generateFormPost $ profileForm u
      renderProfile u profileFormWidget
    Nothing -> do
      setMessage "You must be logged-in to see this page!"
      return $ toHtml ("Nothing" :: Text)

postProfileR :: Handler Html
postProfileR = do
  redirectIfNotLoggedIn HomeR
  newsletter <- lookupPostParam "newsletter"
  u <- getUser
  case u of
   Nothing -> getProfileR
   Just (Entity dbUKey _) ->
     case newsletter of
         Just "yes" -> do
               -- _ <- liftIO $ MC.addSubscriber apiKey listName (unpack $ userEmail user) "newsletter-user" "subscribed"
               _ <- runDB $ setUserForNewsletter (Just 1) dbUKey
               getProfileR
         Just _ -> do
               -- _ <- liftIO $ MC.addSubscriber apiKey listName (unpack $ userEmail user) "newsletter-user" "subscribed"
               _ <- runDB $ setUserForNewsletter Nothing dbUKey
               getProfileR
         Nothing -> do
               -- _ <- liftIO $ MC.removeSubscriber apiKey (unpack $ userEmail user) listName
               _ <- runDB $ setUserForNewsletter Nothing dbUKey
               getProfileR

profileForm :: User -> Form Bool
profileForm User {..} =
  renderDivs $
      areq checkBoxField checkboxSettings (Just (isJust userNewsletter))

renderProfile :: User -> Widget -> Handler Html
renderProfile u widget =
  baseLayout "Profile" Nothing [whamlet|
<section id="content" class="main">
  <header class="major">
    <div>
      <div class="col-md-8">
        <h3> User profile
        <form method="POST" action="@{ProfileR}" name="profile_post">
          <hr />
          ^{widget}
          <hr />
          <input type="hidden" value="#{userEmail u}" name="email" />
          <br />
          <input class="button" type="submit" value="Submit">
      <div class="col-md-4">
        <p>
          <i>
            <b>#{userEmail u}
        <p>
     <div .clearfix>
|]

redirectIfNotLoggedIn :: (RedirectUrl App r) => r -> Handler ()
redirectIfNotLoggedIn r = do
  maybeUser <- getUser
  case maybeUser of
    Nothing -> redirect r
    (Just _) -> pass

checkboxSettings :: FieldSettings master
checkboxSettings = FieldSettings {
    fsLabel = "Newsletter",
    fsTooltip = Just "Newsletter",
    fsId = Nothing,
    fsName = Just "newsletter",
    fsAttrs = []
}
