module Handler.Profile where

import Import

getProfileR :: Handler Html
getProfileR = do
    -- redirectIfNotLoggedIn HomeR
    (profileFormWidget, _) <- generateFormPost profileForm
    renderProfile profileFormWidget

postProfileR :: Handler Html
postProfileR = do
  redirectIfNotLoggedIn HomeR
  ((result, widget), _) <- runFormPost profileForm
  case result of
    FormSuccess email -> error "implement post!"
    _ -> renderProfile widget
profileForm :: Form Bool
profileForm =
  renderDivs $
      areq checkBoxField checkboxSettings (Just True)

renderProfile :: Widget -> Handler Html
renderProfile widget = do
  baseLayout "Profile" Nothing [whamlet|

<section id="content" class="main">
    <div>
      <div .col-md-6>
        <h3> User profile
        <form method="POST" action="@{ProfileR}">
        ^{widget}
         <div .clearfix>
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
    fsAttrs = []
}
