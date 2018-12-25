{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE ViewPatterns          #-}

module Foundation where

import qualified Data.CaseInsensitive as CI
import           Data.Text (Text)
import qualified Data.Text.Encoding as TE
import           Database.Persist.Sql (ConnectionPool, runSqlPool)
import           Handler.Sessions
import           Import.NoFoundation
import           Network.HTTP.Client (Manager, HasHttpManager (..))
import           Text.Hamlet (hamletFile)
import           Text.Jasmine (minifym)
import           Universum
import           Yesod.Core.Types (Logger)
import qualified Yesod.Core.Unsafe as Unsafe
import           Yesod.Default.Util (addStaticContentExternal)
import           Yesod.Static
import           Yesod.Core
import           Yesod.Persist.Core
import           Yesod.Form

data App = App
  { appSettings :: AppSettings
  , appStatic :: Static -- ^ Settings for static file serving.
  , appConnPool :: ConnectionPool -- ^ Database connection pool.
  , appHttpManager :: Manager
  , appLogger :: Logger
  }

type Page = Int

type ArticleSearchString = Text

mkYesodData "App" $(parseRoutesFile "config/routes")

htmlOnly
  :: (MonadHandler m)
  => m Html -> m TypedContent
htmlOnly = selectRep . provideRep

type Form x = Html -> MForm Handler (FormResult x, Widget)

baseLayout :: Html -> Maybe (Entity User) -> Widget -> Handler Html
baseLayout title _ content =
  defaultLayout $ do
    setTitle title
    [whamlet|
^{content}
|]

errorFragment' :: Maybe Text -> Text -> Widget
errorFragment' mmsg t =
  [whamlet|
<div #error-block .container-lg>
  <h1 .error-title>#{t}
  $maybe msg <- mmsg
    <h2 .error-explanation>
      #{msg}
|]

errorFragment :: Text -> Widget
errorFragment = errorFragment' Nothing

instance Yesod App where
  approot =
    ApprootRequest $ \app req ->
     (fromMaybe (getApprootText guessApproot app req)
       (appRoot $ appSettings app))

  makeSessionBackend _ =
    Just <$>
    defaultClientSessionBackend
      120 -- timeout in minutes
      "config/client_session_key.aes"
  yesodMiddleware = defaultYesodMiddleware
  defaultLayout widget = do
    master <- getYesod
    mmsg <- getMessage
    maybeUser <- getUser
    mcurrentRoute <- getCurrentRoute
    (title, parents) <- breadcrumbs
    pc <-
      widgetToPageContent $ do
        addStylesheet $ StaticR css_bootstrap_css
        $(widgetFile "default-layout")
    withUrlRenderer $(hamletFile "templates/default-layout-wrapper.hamlet")
  errorHandler NotFound = do
    user <- getUser
    htmlOnly $
      baseLayout "Not found!" user $
      errorFragment'
        (Just "Sorry, but the page you were looking for could not be found")
        "404 - Page not found"
  errorHandler (InternalError err) = do
    user <- getUser
    htmlOnly $
      baseLayout "Our bad!" user $
      errorFragment' (Just err) "500 - Internal Server Error"
  errorHandler (InvalidArgs _) = do
    user <- getUser
    htmlOnly $
      baseLayout "Invalid request" user $ errorFragment "400 - Bad Request"
  errorHandler NotAuthenticated = do
    user <- getUser
    htmlOnly $
      baseLayout "Not authenticated" user $
      errorFragment' (Just "You are not logged in") "401 - Unauthorized"
  errorHandler (PermissionDenied msg) = do
    user <- getUser
    htmlOnly $
      baseLayout "Permission denied" user $
      errorFragment' (Just msg) "403 - Forbidden"
  errorHandler (BadMethod _) = do
    user <- getUser
    htmlOnly $
      baseLayout "Bad method for request" user $
      errorFragment "400 - Bad Request"
  addStaticContent ext mime content = do
    master <- getYesod
    let staticDir = appStaticDir $ appSettings master
    addStaticContentExternal
      minifym
      genFileName
      staticDir
      (StaticR . flip StaticRoute [])
      ext
      mime
      content
    where
      genFileName lbs = "autogen-" ++ base64md5 lbs
  makeLogger = return . appLogger

instance YesodBreadcrumbs App where
  breadcrumb HomeR = return ("Home", Nothing)
  breadcrumb (StoryListR _) = return ("Articles", Just HomeR)
  breadcrumb (CompanyListR _) = return ("Companies", Just HomeR)
  breadcrumb AboutR = return ("About", Just HomeR)
  breadcrumb SignupR = return ("Signup", Just HomeR)
  breadcrumb LoginR = return ("Login", Just HomeR)
  breadcrumb _ = return ("Home", Nothing)

instance YesodPersist App where
  type YesodPersistBackend App = SqlBackend
  runDB action = do
    master <- getYesod
    runSqlPool action $ appConnPool master

instance YesodPersistRunner App where
  getDBRunner = defaultGetDBRunner appConnPool

instance RenderMessage App FormMessage where
  renderMessage _ _ = defaultFormMessage

instance HasHttpManager App where
  getHttpManager = appHttpManager

unsafeHandler :: App -> Handler a -> IO a
unsafeHandler = Unsafe.fakeHandlerGetLogger appLogger
