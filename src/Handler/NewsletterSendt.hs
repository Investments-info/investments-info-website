{-# LANGUAGE OverloadedStrings #-}
module Handler.NewsletterSendt where

import           II.Newsletter
import           Import
import qualified Text.Blaze.Html5 as H
import           Yadata.LibAPI as YL

reutersUrl :: Text
reutersUrl = "https://www.reuters.com/"

getNewsletterSendtR :: Handler ()
getNewsletterSendtR = do
  redirectIfLoggedIn HomeR
  em <- liftIO sendNewsletter
  case em of
    Left e -> do
      setMessage $ H.text e
      redirect NewsletterManagerR
    Right _ -> do
      setMessage "Emails are on the way!"
      redirect NewsletterManagerR

sendNewsletter :: IO (Either Text ())
sendNewsletter = do
  allStories <- liftIO $ runDBA $ getLatestUniqueStories
  let n = map convertToNews allStories
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
