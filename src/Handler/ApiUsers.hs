{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Handler.ApiUsers where

import           Import hiding (head)
import           Prelude (head)

-- | needed for the servant style handlers to work
instance {-# OVERLAPPABLE #-} (ToJSON a) => ToContent a where
  toContent = toContent . toJSON
instance {-# OVERLAPPABLE #-} (ToJSON a) => ToTypedContent a where
  toTypedContent = TypedContent typeJson . toContent

getApiUsersR :: Handler [User]
getApiUsersR = do
  now <- liftIO getCurrentTime
  return
    [User "a" (Just "isac") Nothing Nothing Nothing Nothing Nothing now]

getFirstUserR :: Handler User
getFirstUserR = head <$> getApiUsersR
