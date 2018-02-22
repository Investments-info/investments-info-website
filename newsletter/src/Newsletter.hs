{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Newsletter where

import           Network.SES (PublicKey (..), Region (USEast1), SESError, SESResult (..),
                              SecretKey (..), sendEmailBlaze)

import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

awsAccessKey= "AKIAI6GDZ5ELIC7ABKJA"
awsSecretKey= "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"

main :: IO ()
main = sendMail >>= \case
         Error e -> putStrLn $ show e
         Success -> putStrLn "Email sent successfully!"

sendMail :: IO SESResult
sendMail = sendEmailBlaze publicKey secretKey region from to subject html
 where
   publicKey = PublicKey "AKIAI6GDZ5ELIC7ABKJA"
   secretKey = SecretKey "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"
   region    = USEast1
   from    = "contact@investments-info.com"
   to      = ["contact@investments-info.com"]
   subject = "Test Subject"
   html = H.html $ do
            H.body $ do
               H.img H.! A.src "http://haskell-lang.org/static/img/logo.png"
               H.h1 "Html email! Hooray"
