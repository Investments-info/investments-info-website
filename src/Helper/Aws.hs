module Helper.Aws
  (listAllEmails
  ) where

import           Control.Lens ((&), (.~))
import           Control.Monad.Trans.AWS (Error, runAWST, trying, _Error)
import           Helper.Helper (getAwsKey)
import           Import
import           Network.AWS (Region (NorthVirginia), newEnv, runResourceT, send, within)
import           Network.AWS.Auth (AccessKey (..), Credentials (FromKeys), SecretKey (..))
import           Network.AWS.SES (IdentityType (..))
import           Network.AWS.SES.ListIdentities (ListIdentitiesResponse, liIdentityType,
                                                 listIdentities)

listAllEmails :: IO (Either Error ListIdentitiesResponse)
listAllEmails = do
  awsAk <- getAwsKey "awsSesAccessKey"
  awsSk <- getAwsKey "awsSesSecretKey"
  env <- newEnv $ FromKeys (AccessKey $ encodeUtf8 awsAk) (SecretKey $ encodeUtf8 awsSk)
  trying _Error (runResourceT . runAWST env . within NorthVirginia $
       send (listIdentities & liIdentityType .~ Just EmailAddress)) :: IO (Either Error ListIdentitiesResponse)
