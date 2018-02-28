{-# LANGUAGE OverloadedStrings #-}
module Handler.NewsletterSend where

import           II.Newsletter
import           Import
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
  allStories <- liftIO $ runDBA $ getLatestUniqueStories
  let n = map convertToNews allStories
  verifiedEmails <- liftIO $ listVerifiedEmails
  _ <- print "verifiedEmails"
  _ <- print verifiedEmails
  liftIO $
    YL.createGraphForNewsletter
      ["IBM", "MSFT", "AAPL", "KO"]
      "static/newsletter-graph.jpg"
  emailingResult <- sesEmail ["contact@investments-info.com"] n
  case emailingResult of
    Left e  -> return $ Left e
    Right _ -> return $ Right ()

convertToNews :: Entity Story -> News
convertToNews sl = News t l
  where
    link = (storyLink $ entityVal sl)
    httplink = reutersUrl <> (storyLink $ entityVal sl)
    t = (storyTitle $ entityVal sl)
    l =
      if isInfixOf "http" link
        then link
        else httplink

redirectIfLoggedIn
  :: (RedirectUrl App r)
  => r -> Handler ()
redirectIfLoggedIn r = do
  maybeUser <- getUser
  case maybeUser of
    Nothing  -> return ()
    (Just _) -> redirect r
