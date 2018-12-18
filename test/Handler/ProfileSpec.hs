module Handler.ProfileSpec
  ( spec
  ) where

import           TestImport
import           Universum hiding (get)
import qualified Data.Text as T

spec :: Spec
spec =
  withApp $
    describe "Profile page" $ do
      it "asserts no access to my-account for anonymous users" $ do
        get ProfileR
        statusIs 403
      -- it "asserts access to my-account for authenticated users" $ do
      --   -- userEntity <- createUser "foo"
      --   -- authenticateAs userEntity
      --   get ProfileR
      --   statusIs 200
      -- it "asserts user's information is shown" $ do
      --   -- userEntity <- lift $ createUser "bar"
      --   -- authenticateAs userEntity
      --   get ProfileR
      --   let (Entity _ user) = userEntity
      --   htmlAnyContain ".username" $ T.unpack (fromMaybe "" (userName user))
