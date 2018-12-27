module Handler.HomeSpec
  ( spec
  ) where

import           TestImport
import           Universum hiding (get)

spec :: Spec
spec =
  withApp $
    describe "Homepage" $ do
      it "loads the index and checks it looks right" $ do
        get HomeR
        statusIs 200
        htmlAnyContain "h2" "Financial News"
