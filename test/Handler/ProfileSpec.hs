module Handler.ProfileSpec
  ( spec
  ) where

import           TestImport
import           Universum hiding (get)

spec :: Spec
spec =
  withApp $
    describe "Profile page" $
      it "asserts no access to my-account for anonymous users" $ do
        get ProfileR
        statusIs 303
