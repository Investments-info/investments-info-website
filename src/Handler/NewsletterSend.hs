{-# LANGUAGE OverloadedStrings #-}

module Handler.NewsletterSend where

import           Control.Lens
import           Helper.Aws (listAllEmails)
import           Helper.Helper (getAwsKey)
import           II.Newsletter
import           Import
import           Network.AWS.SES.ListIdentities (lirsIdentities)
import qualified Text.Blaze.Html5 as H
import           Yadata.LibAPI as YL

reutersUrl :: Text
reutersUrl = "https://www.reuters.com/"

getNewsletterSendR :: Handler ()
getNewsletterSendR = do
  _ <- requireAdmin
  em <- liftIO sendNewsletter
  case em of
    Left e -> do
      setMessage $ H.text e
      redirect NewsletterManagerR
    Right _ -> do
      setMessage "Emails are on the way!"
      redirect AdminR

sendNewsletter :: IO (Either Text ())
sendNewsletter = do
  allStories <- liftIO $ runDBA getLatestUniqueStories
  let n = map convertToNews allStories
  liftIO $
    YL.createGraphForNewsletter
      ["IBM", "MSFT", "AAPL", "KO"]
      "static/newsletter-graph.jpg"
  emails <- listAllEmails
  case emails of
    Right eList -> do
      awsAk <- getAwsKey "awsSesAccessKey"
      awsSk <- getAwsKey "awsSesSecretKey"
      emailingResult <-
        sesEmail
          (encodeUtf8 awsAk)
          (encodeUtf8 awsSk)
          (map (encodeUtf8 . fromStrict) (eList ^. lirsIdentities))
          n
      case emailingResult of
        Left e  -> return $ Left e
        Right _ -> return $ Right ()
    Left e -> return $ Left $ pack (show e)

convertToNews :: Entity Story -> News
convertToNews sl = News t l
  where
    link = storyLink $ entityVal sl
    httplink = reutersUrl <> storyLink (entityVal sl)
    t = storyTitle $ entityVal sl
    l =
      if "http" `isInfixOf` link
        then link
        else httplink

redirectIfLoggedIn
  :: (RedirectUrl App r)
  => r -> Handler ()
redirectIfLoggedIn r = do
  maybeUser <- getUser
  case maybeUser of
    Nothing  -> pass
    (Just _) -> redirect r
