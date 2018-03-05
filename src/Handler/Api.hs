module Handler.Api where

import           Import hiding (head)

getApiR :: Handler Value
getApiR = return $ String "api v0.0.1"
