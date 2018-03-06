{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE GADTs             #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module II.Newsletter
  ( sesEmail
  , verifyEmail
  , listVerifiedEmails
  , News(..)
  ) where

import           Control.Applicative
import           Control.Exception (SomeException, try)
import           Control.Monad.Error
import           Crypto.Hash (Digest, SHA256, hmac, hmacGetDigest)
import           Data.Byteable (toBytes)
import           Data.ByteString (ByteString)
import           Data.ByteString.Base64 (encode)
import           Data.ByteString.Char8 (concat, pack, unpack)
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as L
import           Data.Monoid ((<>))
import           Data.Text (Text, pack)
import qualified Data.Text as T
import           Data.Text.Lazy (fromStrict)
import           Data.Text.Lazy.Encoding (encodeUtf8)
import           Data.Time.Clock (getCurrentTime)
import           Data.Time.Format (defaultTimeLocale, formatTime)
import           Network.Http.Client (Connection, Method (GET, POST), baselineContextSSL,
                                      buildRequest, closeConnection, encodedFormBody, http,
                                      openConnectionSSL, sendRequest, setContentType, setHeader)
import           Network.SES (PublicKey (..), Region (USEast1), SESErrorType (..), SESResult (..),
                              SecretKey (..), sendEmailBlaze)
import           OpenSSL (withOpenSSL)
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

data AwsActions a where
        ListAwsIdentities :: AwsActions a
        VerifyAwsIdentity :: ByteString -> AwsActions a

awsAccessKey :: ByteString
awsAccessKey = "AKIAI6GDZ5ELIC7ABKJA"

awsSecretKey :: ByteString
awsSecretKey = "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"

type ConnectionError a = IO (Either SomeException a)

data News = News
  { _nTitle :: Text
  , _nLink  :: Text
  } deriving (Show)

data VErr = VErr Text deriving (Eq, Show)
data VResult = VResult Text deriving (Eq, Show)

data VerifiedEmailsRes a =
    VerifiedEmailsRes
    { verror  :: VErr
    , vresult :: VResult
    } deriving (Eq, Show, Functor, Applicative, Monad, MonadIO, MonadError VErr)

sesEmail :: [L.ByteString] -> [News] -> IO (Either Text Text)
sesEmail to n =
  sendMail to n >>= \case
    Error e -> return $ Left $ T.pack (show e)
    Success -> return $ Right (T.pack "success")

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
    links =
      linesToHtml $
      map
        (\x ->
           H.html $ do
             H.a H.! A.href (H.textValue (_nLink x)) $ (H.toHtml (_nTitle x))
             H.br)
        n
    html =
      H.html $ do
        H.body $ do
          H.h3 "Investments Info Newsletter"
          H.br
          links
          H.br
          H.img H.! A.src "http://localhost:3000/static/newsletter-graph.jpg"

-- | Render the lines as HTML lines.
linesToHtml :: [H.Html] -> H.Html
linesToHtml = htmlIntercalate H.br

-- | Intercalate the given things.
htmlIntercalate :: H.Html -> [H.Html] -> H.Html
htmlIntercalate _ [x] = x
htmlIntercalate sep (x:xs) = do
  x
  sep
  htmlIntercalate sep xs
htmlIntercalate _ [] = mempty

verifyEmail :: Text -> IO ByteString
verifyEmail email = makeRequest publicKey secretKey region requestMethod query
  where
    publicKey = PublicKey awsAccessKey
    secretKey = SecretKey awsSecretKey
    region = USEast1
    action = "VerifyEmailIdentity"
    requestMethod = POST
    query =
      generateQueryString $
      VerifyAwsIdentity (L.toStrict (encodeUtf8 (fromStrict email)))

listVerifiedEmails :: IO ByteString
listVerifiedEmails = makeRequest publicKey secretKey region requestMethod query
  where
    publicKey = PublicKey awsAccessKey
    secretKey = SecretKey awsSecretKey
    region = USEast1
    action = "ListIdentities"
    requestMethod = POST
    query = generateQueryString ListAwsIdentities

generateQueryString :: AwsActions a -> [(ByteString, ByteString)]
generateQueryString (VerifyAwsIdentity a) =
  ("Action", "VerifyEmailIdentity") : ("EmailAddress", a) : []
generateQueryString ListAwsIdentities =
  ("Action", "ListIdentities") : ("IdentityType", "EmailAddress") : []

makeSig
  :: ByteString -- ^ Payload
  -> ByteString -- ^ Key
  -> ByteString
makeSig payload key =
  encode $ toBytes (hmacGetDigest $ hmac key payload :: Digest SHA256)

makeRequest
  :: PublicKey -- ^ AWS Public Key
  -> SecretKey -- ^ AWS Secret Key
  -> Region -- ^ The Region to send the Request
  -> Method -- ^ Request type POST || GET
  -> [(ByteString, ByteString)] -- ^ Api query
  -> IO ByteString
makeRequest (PublicKey publicKey) (SecretKey secretKey) region requestMethod query =
  withOpenSSL $ do
    now <- getCurrentTime
    let date = C8.pack $ format now
        sig = makeSig date secretKey
        format = formatTime defaultTimeLocale "%a, %e %b %Y %H:%M:%S %z"
        auth =
          C8.concat
            [ "AWS3-HTTPS AWSAccessKeyId="
            , publicKey
            , ", Algorithm=HmacSHA256, Signature="
            , sig
            ]
        queryString = query
    req <-
      buildRequest $ do
        http requestMethod "/"
        setContentType "application/x-www-form-urlencoded"
        setHeader "X-Amzn-Authorization" auth
        setHeader "Date" date
    ctx <- baselineContextSSL
    let
    connResult <-
      try
        (openConnectionSSL
           ctx
           ("email." <> C8.pack (show region) <> ".amazonaws.com")
           443) :: ConnectionError Connection
    case connResult of
      Left s -> connectionError s
      Right con -> do
        result <-
          try (sendRequest con req $ encodedFormBody queryString) :: ConnectionError ()
        case result of
          Left s -> connectionError s
          Right a -> do
            closeConnection con
            return . C8.pack . show $ a
     where
       connectionError = return . C8.pack . show
