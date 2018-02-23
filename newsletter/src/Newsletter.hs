{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Newsletter where

import           Control.Exception.Safe (Exception, displayException)
import           Control.Monad.Error (ErrorT, runErrorT)
import           Control.Monad.Except (ExceptT, lift, throwError)
import           Data.ByteString (ByteString)
import           Data.Text (Text, pack, unpack)
import           Network.SES (PublicKey (..), Region (USEast1), SESError (..), SESResult (..),
                              SecretKey (..), sendEmailBlaze)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

awsAccessKey :: ByteString
awsAccessKey = "AKIAI6GDZ5ELIC7ABKJA"

awsSecretKey :: ByteString
awsSecretKey = "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"

data SesException = SesException Text deriving Show

instance Exception SesException

printSesError :: SESError -> Text
printSesError  er =
    case er of
       SESConnectionError e -> pack e
       SESError _ _ e       -> pack e


main :: IO Text
main =
  sendMail >>= \case
    Error e -> return $ printSesError e
    Success -> return ""

sendMail :: IO SESResult
sendMail = sendEmailBlaze publicKey secretKey region from to subject html
  where
    publicKey = PublicKey awsAccessKey
    secretKey = SecretKey awsSecretKey
    region = USEast1
    from = "contact@investments-info.com"
    to = ["contact@investments-info.com"]
    subject = "Test Subject"
    html =
      H.html $ do
        H.body $ do
          H.img H.! A.src "http://haskell-lang.org/static/img/logo.png"
          H.h1 "Html email! Hooray"
