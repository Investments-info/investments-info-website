module Helper.Aws
  (listAllEmails
  ) where

import           Control.Lens ((&), (.~))
import           Control.Monad.Trans.AWS (Error, runAWST, trying, _Error)
import           Data.ByteString (ByteString)
import           Network.AWS (Region (NorthVirginia), newEnv, runResourceT, send, within)
import           Network.AWS.Auth (AccessKey (..), Credentials (FromKeys), SecretKey (..))
import           Network.AWS.SES (IdentityType (..))
import           Network.AWS.SES.ListIdentities (ListIdentitiesResponse, liIdentityType,
                                                 listIdentities)
import           Prelude (Either (..), IO, Maybe (..), ($), (.))


awsAccessKey :: ByteString
awsAccessKey = "AKIAI6GDZ5ELIC7ABKJA"

awsSecretKey :: ByteString
awsSecretKey = "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"

listAllEmails :: IO (Either Error ListIdentitiesResponse)
listAllEmails = do
  env <- newEnv $ FromKeys (AccessKey awsAccessKey) (SecretKey awsSecretKey)
  trying _Error (runResourceT . runAWST env . within NorthVirginia $
       send (listIdentities & liIdentityType .~ (Just EmailAddress) )) :: IO (Either Error ListIdentitiesResponse)
