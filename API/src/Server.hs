{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Server where

import           Control.Monad.Except
import           Data.Aeson.Compat
import           GHC.Generics
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Prelude ()
import           Prelude.Compat
import           Servant
import           Servant.HTML.Blaze
import           Text.Blaze.Html5 hiding (main)

-- | Home
data Home =
  Home

home :: Proxy Home
home = Proxy

instance ToMarkup Home where
  toMarkup Home = h1 "Investments Info API"

-- | Companies
data Company = Company
  { name :: String
  } deriving (Eq, Show, Generic)

instance ToJSON Company

companies :: [Company]
companies = [Company "Apple", Company "IBM"]

-- | API
type ApiSchema = Get '[ HTML] Home :<|> "company" :> Get '[ JSON] [Company]

appAPI :: Proxy ApiSchema
appAPI = Proxy

appServer :: Server ApiSchema
appServer = return $
    home :<|> companies
    where home =

app :: Application
app = serve appAPI appServer

main :: IO ()
main = run 8081 app
