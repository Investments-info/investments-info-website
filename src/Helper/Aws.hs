{-# LANGUAGE ViewPatterns #-}
module Helper.Aws (sesMail) where

import Control.Monad
import Crypto.Hash (Digest, SHA256, digestToHexByteString, hash, hmac, hmacGetDigest)
import Data.Aeson
import Data.Byteable (toBytes)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16 (encode)
import qualified Data.ByteString.Char8 as C
import Data.CaseInsensitive (original)
import Data.Char (toLower)
import Data.List (intersperse, sortBy)
import Data.Monoid ((<>))
import Data.Text (Text, unpack)
import Data.Time (getCurrentTime)
import Data.Time.Clock (UTCTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import Network.HTTP.Client hiding (httpLbs)
import Network.HTTP.Client.TLS
import Network.HTTP.Conduit
import Network.HTTP.Types
import Prelude

data AwsCreds =
    AwsCreds
    { awsK :: ByteString
    , awsS :: ByteString
    }

data SendEmailRequest = SendEmailRequest
    { region          :: ByteString
    , accessKeyId     :: ByteString
    , secretAccessKey :: ByteString
    , source          :: ByteString
    , to              :: [ByteString]
    , subject         :: ByteString
    , body            :: ByteString
    } deriving Show

data SendEmailResponse = SendEmailResponse
    { _requestId :: Text
    , messageId  :: Text
    } deriving Show

instance FromJSON SendEmailResponse where
    parseJSON (Object o) = do
        response <- o .: "SendEmailResponse"
        reqId <- response .: "ResponseMetadata" >>= (.: "RequestId")
        msgId <- response .: "SendEmailResult" >>= (.: "MessageId")
        return $ SendEmailResponse reqId msgId
    parseJSON _ = mzero

mkAwsCreds :: AwsCreds
mkAwsCreds =
    AwsCreds
    { awsK = "AKIAINRBN4RMRLKL4USA"
    , awsS = "kdStbg7HBQZxTs9Oo+kK8vGZavxvFjIyDv7lUc8s"
    }

canonicalRequest :: Request -> ByteString -> ByteString
canonicalRequest req bbody = C.concat $
    intersperse "n"
        [ method req
        , path req
        , queryString req
        , canonicalHeaders req
        , signedHeaders req
        , hexHash bbody
        ]

hexHash :: ByteString -> ByteString
hexHash p = digestToHexByteString (hash p :: Digest SHA256)

headers :: Request -> [Header]
headers req = sortBy (\(a,_) (b,_) -> compare a b) (("host", host req) : requestHeaders req)

canonicalHeaders :: Request -> ByteString
canonicalHeaders req =
    C.concat $ map (\(hn,hv) -> bsToLower (original hn) <> ":" <> hv <> "n") hs
  where hs = headers req

bsToLower :: ByteString -> ByteString
bsToLower = C.map toLower

signedHeaders :: Request -> ByteString
signedHeaders req =
    C.concat . intersperse ";"  $ map (\(hn,_) -> bsToLower (original hn)) hs
  where hs = headers req

v4DerivedKey :: ByteString -> -- ^ AWS Secret Access Key
                ByteString -> -- ^ Date in YYYYMMDD format
                ByteString -> -- ^ AWS region
                ByteString -> -- ^ AWS service
                ByteString
v4DerivedKey bsecretAccessKey date bregion service = hmacSHA256 kService "aws4_request"
  where kDate = hmacSHA256 ("AWS4" <> bsecretAccessKey) date
        kRegion = hmacSHA256 kDate bregion
        kService = hmacSHA256 kRegion service

hmacSHA256 :: ByteString -> ByteString -> ByteString
hmacSHA256 key p = toBytes $ (hmacGetDigest $ hmac key p :: Digest SHA256)

stringToSign :: UTCTime    -> -- ^ current time
                ByteString -> -- ^ The AWS region
                ByteString -> -- ^ The AWS service
                ByteString -> -- ^ Hashed canonical request
                ByteString
stringToSign date bregion service hashConReq = C.concat
    [ "AWS4-HMAC-SHA256n"
    , C.pack (formatAmzDate date) , "n"
    , C.pack (format date) , "/"
    , bregion , "/"
    , service
    , "/aws4_requestn"
    , hashConReq
    ]

format :: UTCTime -> String
format = formatTime defaultTimeLocale "%Y%m%d"

formatAmzDate :: UTCTime -> String
formatAmzDate = formatTime defaultTimeLocale "%Y%m%dT%H%M%SZ"

createSignature ::  Request         -> -- ^ Http request
                    ByteString      -> -- ^ Body of the request
                    UTCTime         -> -- ^ Current time
                    ByteString      -> -- ^ Secret Access Key
                    ByteString      -> -- ^ AWS region
                    ByteString
createSignature req bbody now key bregion = v4Signature dKey toSign
  where canReqHash = hexHash $ canonicalRequest req bbody
        toSign = stringToSign now bregion "ses" canReqHash
        dKey = v4DerivedKey key (C.pack $ format now) bregion "ses"

v4Signature :: ByteString -> ByteString -> ByteString
v4Signature derivedKey payLoad = B16.encode $ hmacSHA256 derivedKey payLoad

_usEast1 :: ByteString
_usEast1 = "us-east-1"

_usWest2 :: ByteString
_usWest2 = "us-west-2"

euWest1 :: ByteString
euWest1 = "eu-west-1"

sendEmail :: SendEmailRequest -> IO (Either String SendEmailResponse)
sendEmail sendReq = do
    fReq <- parseUrlThrow $ "https://email." ++ C.unpack (region sendReq) ++ ".amazonaws.com"
    now <- getCurrentTime
    manager <- newManager $ managerSetProxy noProxy tlsManagerSettings
    setGlobalManager manager
    let req = fReq
                { requestHeaders =
                    [ ("Accept", "text/json")
                    , ("Content-Type", "application/x-www-form-urlencoded")
                    , ("x-amz-date", C.pack $ formatAmzDate now)
                    ]
                , method = "POST"
                , requestBody = RequestBodyBS reqBody
                }
        -- sig = createSignature req reqBody now (secretAccessKey sendReq) (region sendReq)
    resp <- httpLbs (authenticateRequest sendReq now req reqBody) manager
    case responseStatus resp of
        (Status 200 _) -> return $ eitherDecode (responseBody resp)
        (Status code msg) ->
            return $ Left ("Request failed with status code <" ++
                show code ++ "> and message <" ++ C.unpack msg ++ ">")
  where
    reqBody = renderSimpleQuery False $
                    [ ("Action", "SendEmail")
                    , ("Source", source sendReq)
                    ] ++ toAddressQuery (to sendReq) ++
                    [ ("Message.Subject.Data", subject sendReq)
                    , ("Message.Body.Text.Data", body sendReq)
                    ]

authenticateRequest :: SendEmailRequest -> UTCTime -> Request -> ByteString -> Request
authenticateRequest sendReq now req bbody =
    req { requestHeaders =
            authHeader now (accessKeyId sendReq)
                           (signedHeaders req) sig
                           (region sendReq) :
                           (requestHeaders req)
        }
  where sig = createSignature req bbody now (secretAccessKey sendReq) (region sendReq)

toAddressQuery :: [ByteString] -> SimpleQuery
toAddressQuery tos =
    zipWith (\index address ->
                ( "Destination.ToAddresses.member." <>
                    C.pack (show index)
                , address)
            ) ([1..] :: [Int]) tos

authHeader ::   UTCTime     -> -- ^ Current time
                ByteString  -> -- ^ Secret access key
                ByteString  -> -- ^ Signed headers
                ByteString  -> -- ^ Signature
                ByteString  -> -- ^ AWS Region
                Header
authHeader now sId signHeads sig bregion =
    ( "Authorization"
    , C.concat
        [ "AWS4-HMAC-SHA256 Credential="
        , sId , "/"
        , C.pack (format now) , "/"
        , bregion
        , "/ses/aws4_request, SignedHeaders="
        , signHeads
        , ", Signature="
        , sig
        ]
    )

sesMail :: [ByteString] -> ByteString -> ByteString -> IO ()
sesMail bto bsub bcontent = do
    let aws  = mkAwsCreds
    let awsId = awsK aws
    let awsSecret = awsS aws
    let sendRequest = SendEmailRequest euWest1 awsId awsSecret "contact@investments-info.com"
                        bto bsub bcontent
    response <- sendEmail sendRequest
    case response of
        Left err -> putStrLn $ "Failed to send : " ++ err
        Right resp ->
            putStrLn $ "Successfully sent with message ID : " ++ unpack (messageId resp)
