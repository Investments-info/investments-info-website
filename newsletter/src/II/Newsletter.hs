{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module II.Newsletter
  ( sesEmail
  , News (..)
  ) where

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import           Data.Text (Text, pack)
import           Network.SES (PublicKey (..), Region (USEast1), SESResult (..), SecretKey (..),
                              sendEmailBlaze)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

awsAccessKey :: ByteString
awsAccessKey = "AKIAI6GDZ5ELIC7ABKJA"

awsSecretKey :: ByteString
awsSecretKey = "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"

data News = News
  { _nTitle :: Text
  , _nLink  :: Text
  } deriving (Show)

sesEmail :: [L.ByteString] -> [News] -> IO (Either Text Text)
sesEmail to n =
  sendMail to n >>= \case
    Error e -> return $ Left $ pack (show e)
    Success -> return $ Right (pack "success")

sendMail :: [L.ByteString] -> [News] -> IO SESResult
sendMail emailList n =
  sendEmailBlaze publicKey secretKey region from to subject html
  where
    publicKey = PublicKey awsAccessKey
    secretKey = SecretKey awsSecretKey
    region = USEast1
    from = "contact@investments-info.com"
    subject = "Investments Info Newsletter"
    to = emailList
    links =  linesToHtml $ map (\x -> H.a H.! A.href (H.textValue (_nLink x)) $ (H.toHtml (_nTitle x)) ) n
    html =
      H.html $ do
        H.body $ do
           links

-- | Render the lines as HTML lines.
linesToHtml :: [H.Html] -> H.Html
linesToHtml = htmlIntercalate H.br

-- | Intercalate the given things.
htmlIntercalate :: H.Html -> [H.Html] -> H.Html
htmlIntercalate _ [x]      = x
htmlIntercalate sep (x:xs) = do x; sep; htmlIntercalate sep xs
htmlIntercalate _ []       = mempty
