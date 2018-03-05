{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
  ( startApp
  , app
  ) where

import           Data.Aeson
import           Data.Aeson.TH
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Servant
import           Servant.HTML.Blaze
import           Text.Blaze.Html5 hiding (main)

data User = User
  { userId        :: Int
  , userFirstName :: String
  , userLastName  :: String
  } deriving (Eq, Show)

$(deriveJSON defaultOptions ''User)

type API =  "home" :> Get '[HTML] Html :<|> "users" :> Get '[JSON] [User]

startApp :: IO ()
startApp = run 8080 app

app :: Application
app = serve api server

api :: Proxy API
api = Proxy

server :: Server API
server = home :<|> users
  where
    home :: Handler Html
    home = return $ h1 "Investments Info API"
    users :: Handler [User]
    users = return [User 1 "Isaac" "Newton", User 2 "Albert" "Einstein"]
