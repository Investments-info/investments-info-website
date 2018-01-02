{-# LANGUAGE RecordWildCards #-}
module Handler.Profile where

import Import
import MailchimpSimple as MC

-- move this to helper
listName :: String
listName = "investments-info"

apiKey :: String
apiKey = "ab4685034f82cdd3c97286e4839b7cee-us17"

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
  Just (Entity dbUKey user) <- getUser
  case newsletter of
      Just "yes" -> do
            _ <- liftIO $ MC.addSubscriber apiKey listName (unpack $ userEmail user) "newsletter-user" "subscribed"
            _ <- runDB $ setUserForNewsletter (Just 1) dbUKey
            getProfileR
      Just _ -> do
            _ <- liftIO $ MC.addSubscriber apiKey listName (unpack $ userEmail user) "newsletter-user" "subscribed"
            _ <- runDB $ setUserForNewsletter Nothing dbUKey
            getProfileR
      Nothing -> do
            _ <- liftIO $ MC.removeSubscriber apiKey (unpack $ userEmail user) listName
            _ <- runDB $ setUserForNewsletter Nothing dbUKey
            getProfileR

profileForm :: User -> Form Bool
profileForm User {..} =
  renderDivs $
      areq checkBoxField checkboxSettings (Just (isJust userNewsletter))

renderProfile :: User -> Widget -> Handler Html
renderProfile u widget = do
  baseLayout "Profile" Nothing [whamlet|

<section id="content" class="main">
    <div>
      <div .col-md-6>
        <p .pull-right>
          <i>
            <b>#{userEmail u}
        <h3> User profile
        <form method="POST" action="@{ProfileR}" name="profile_post">
          ^{widget}
          <input type="hidden" value="#{userEmail u}" name="email" />
          <input .button type="submit" value="Submit">
     <div .clearfix>
|]

redirectIfNotLoggedIn :: (RedirectUrl App r) => r -> Handler ()
redirectIfNotLoggedIn r = do
  maybeUser <- getUser
  case maybeUser of
    Nothing -> redirect r
    (Just _) -> return ()

checkboxSettings :: FieldSettings master
checkboxSettings = FieldSettings {
    fsLabel = "Newsletter",
    fsTooltip = Just "Newsletter",
    fsId = Nothing,
    fsName = Just "newsletter",
    fsAttrs = [("class", "form-control")]
}
