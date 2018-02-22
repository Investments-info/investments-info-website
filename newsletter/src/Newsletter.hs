{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Newsletter where

import           Control.Exception.Safe (Exception)
import           Control.Monad.Except (ExceptT, lift, throwError)
import           Data.Text (Text, pack)
import           Network.SES (PublicKey (..), Region (USEast1), SESError, SESResult (..),
                              SecretKey (..), sendEmailBlaze)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

awsAccessKey :: Text
awsAccessKey = "AKIAI6GDZ5ELIC7ABKJA"

awsSecretKey :: Text
awsSecretKey = "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"

data SesException = SesException Text deriving Show

instance Exception SesException

main :: ExceptT Text IO ()
main =
  lift $ sendMail >>= \case
    Error e -> throwError $ SesException $ pack (show e)
    Success -> return ()

sendMail :: IO SESResult
sendMail = sendEmailBlaze publicKey secretKey region from to subject html
  where
    publicKey = PublicKey "AKIAI6GDZ5ELIC7ABKJA"
    secretKey = SecretKey "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"
    region = USEast1
    from = "contact@investments-info.com"
    to = ["contact@investments-info.com"]
    subject = "Test Subject"
    html =
      H.html $ do
        H.body $ do
          H.img H.! A.src "http://haskell-lang.org/static/img/logo.png"
          H.h1 "Html email! Hooray"
