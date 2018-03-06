{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Handler.ApiCompanies where

import           Import

-- | needed for the servant style handlers to work
instance {-# OVERLAPPABLE #-} (ToJSON a) => ToContent a where
  toContent = toContent . toJSON
instance {-# OVERLAPPABLE #-} (ToJSON a) => ToTypedContent a where
  toTypedContent = TypedContent typeJson . toContent

getApiCompaniesR :: Handler [Company]
getApiCompaniesR = do
  now <- liftIO getCurrentTime
  cmps <- runDB allCompanies
  return $ map entityVal cmps

