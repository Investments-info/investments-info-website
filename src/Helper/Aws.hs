module Helper.Aws where

import Control.Monad.Trans.AWS
import Import hiding (Env)
import Network.AWS.Auth

createCredentials :: Handler Env
createCredentials = do
  env <- newEnv $ FromKeys (AccessKey awsAccessKey) (SecretKey awsSecretKey)
  return env
