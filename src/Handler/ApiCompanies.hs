{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module Handler.ApiCompanies where

import           Import

newtype ApiCompany a = ApiCompany { companyName :: String } deriving (Eq,Show)

instance {-# OVERLAPPABLE #-} (ToJSON (ApiCompany Company)) => ToContent (ApiCompany Company) where
  toContent = toContent . toJSON
instance {-# OVERLAPPABLE #-} (ToJSON (ApiCompany Company)) => ToTypedContent (ApiCompany Company) where
  toTypedContent = TypedContent typeJson . toContent

getApiCompaniesR :: Handler Value
getApiCompaniesR = do
  cmps <- runDB allCompanies
  returnJson $ map entityVal cmps

