module Handler.Api where

import           Import

getApiR :: Handler Value
getApiR = return $ String "api v0.0.1"
