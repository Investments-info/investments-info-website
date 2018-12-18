module Handler.CommonSpec
  ( spec
  ) where

import           TestImport

spec :: Spec
spec =
  withApp $ do
    describe "robots.txt" $ do
      it "gives a 200" $
        get RobotsR
        statusIs 200
      it "has correct User-agent" $ do
        get RobotsR
        bodyContains "User-agent: *"
    describe "favicon.ico" $
      it "gives a 200" $
        get FaviconR
        statusIs 200
