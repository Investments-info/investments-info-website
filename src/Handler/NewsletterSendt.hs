{-# LANGUAGE OverloadedStrings #-}

module Handler.NewsletterSendt where

import           Helper.Helper (getAwsKey)
import           II.Newsletter (News (..), sesEmail)
import           Import
import qualified Text.Blaze.Html5 as H
import           Yadata.LibAPI as YL

reutersUrl :: Text
reutersUrl = "https://www.reuters.com/"

getNewsletterSendtR :: Handler ()
getNewsletterSendtR = do
  _ <- requireAdmin
  em <- liftIO sendNewsletter
  case em of
    Left e -> do
      setMessage $ H.text e
      redirect NewsletterManagerR
    Right _ -> do
      setMessage "Test Email is on the way!"
      redirect AdminR

sendNewsletter :: IO (Either Text ())
sendNewsletter = do
  allStories <- liftIO $ runDBA getLatestUniqueStories
  let n = map convertToNews allStories
  liftIO $
    YL.createGraphForNewsletter
      ["IBM", "MSFT", "AAPL", "KO"]
      "static/newsletter-graph.jpg"
  awsAk <- getAwsKey "awsSesAccessKey"
  awsSk <- getAwsKey "awsSesSecretKey"
  emailingResult <-
    sesEmail
      (encodeUtf8 awsAk)
      (encodeUtf8 awsSk)
      ["contact@investments-info.com"]
      n
  case emailingResult of
    Left e  -> return $ Left e
    Right _ -> return $ Right ()

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
    Nothing -> do
      setMessage "You are not authorized to do this!"
      pass
    (Just _) -> redirect r
