{-# LANGUAGE OverloadedStrings #-}
module Text.HTML.Fscraper where

import Text.HTML.Scalpel
import Network.Curl
import Control.Applicative ((<|>))

reutersUrl :: String
reutersUrl = "http://www.reuters.com/finance/markets"

type NewsLink = String
type NewsTitle = String
type NewsImage = String
type NewsText = String

data News
  = TextNews { newstitle :: NewsTitle
             , newslink :: NewsLink
             , newstext :: NewsText }
  | ImageNews { newsimage :: NewsImage
              , newstitle :: NewsTitle
              , newslink :: NewsLink
              , newstext :: NewsText }
  deriving (Show, Eq)

topStory :: String -> String -> IO (Maybe [News])
topStory classname url = scrapeURL url topStoryNews
  where
    topStoryNews :: Scraper String [News]
    topStoryNews = chroots ("div" @: [hasClass classname]) topStoryImageNews

featureNews :: String -> String -> IO (Maybe [News])
featureNews classname url = scrapeURL url storyNews
  where
    storyNews :: Scraper String [News]
    storyNews = chroots ("div" @: [hasClass classname]) featureStoryImageNews

leftColumnNews :: String -> String -> IO (Maybe [News])
leftColumnNews classname url = scrapeURL url newslist
  where
    newslist :: Scraper String [News]
    newslist = chroots ("div" @: [hasClass classname] // (tagSelector "li")) sideColumnTextNews

topStoryImageNews :: Scraper String News
topStoryImageNews = do
  title <- text $ "h2"
  link <- attr "href" $ "a"
  imageURL <- attr "src" $ "img"
  ntext <- text $ "p"
  return $ ImageNews imageURL title link ntext

sideColumnTextNews :: Scraper String News
sideColumnTextNews = do
  title <- text $ "a"
  link <- attr "href" $ "a"
  ntext <- text $ "span" @: [hasClass "timestamp"]
  return $ TextNews title link ntext

featureStoryImageNews :: Scraper String News
featureStoryImageNews = do
  title <- text $ "h2" @: [hasClass "story-title"]
  link <- attr "href" $ "a"
  imageURL <- attr "org-src" $ "img"
  return $ ImageNews imageURL title link ""

debugger = do
    html <- scrapeURLWithOpts opts url $ htmls (tagSelector "ul")
    maybe printError printHtml html
  where
    url = "http://www.reuters.com/finance/markets"
    opts = [ CurlUserAgent "some user agent string" ]
    printError = putStrLn "Failed"
    printHtml = mapM_ putStrLn
