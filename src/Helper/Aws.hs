{-# LANGUAGE ViewPatterns #-}

module Helper.Aws
  (listAllEmails
  ) where

import           Control.Monad.Trans.AWS (runAWST)
import           Data.ByteString (ByteString)
import           Network.AWS (Region (NorthVirginia), newEnv, runResourceT, within)
import           Network.AWS.Auth (AccessKey (..), Credentials (FromKeys), SecretKey (..))
import           Network.AWS.SES.ListIdentities (listIdentities)
import           Prelude


awsAccessKey :: ByteString
awsAccessKey = "AKIAI6GDZ5ELIC7ABKJA"

awsSecretKey :: ByteString
awsSecretKey = "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"

listAllEmails :: IO ()
listAllEmails = do
  env <- newEnv $ FromKeys (AccessKey awsAccessKey) (SecretKey awsSecretKey)
  emails <-
    runResourceT . runAWST env . within NorthVirginia $ do
      return listIdentities
  print emails
