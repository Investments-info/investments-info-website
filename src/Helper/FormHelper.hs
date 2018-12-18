module Helper.FormHelper where

import           Data.Text (Text)
import           Data.Time (UTCTime)
import           Import
import           Universum

titleSettings :: FieldSettings master
titleSettings = FieldSettings {
    fsLabel = "Title",
    fsTooltip = Just "Title",
    fsId = Nothing,
    fsName = Just "title",
    fsAttrs = [("autofocus", "true"),("class","form-control")]
}

contentSettings :: FieldSettings master
contentSettings = FieldSettings {
    fsLabel = "Content",
    fsTooltip = Just "Content",
    fsId = Nothing,
    fsName = Just "content",
    fsAttrs = [("class","form-control")]
}

defaultFormSettings :: SomeMessage master -> FieldSettings master
defaultFormSettings t = FieldSettings {
    fsLabel = t,
    fsTooltip = Nothing,
    fsId = Nothing,
    fsName = Nothing,
    fsAttrs = [("class","form-control")]
}

specialFormSettings :: SomeMessage master -> SomeMessage master  -> Text -> Text -> Text -> FieldSettings master
specialFormSettings label tooltip idname name classname = FieldSettings {
    fsLabel = label,
    fsTooltip = Just tooltip,
    fsId = Just idname,
    fsName = Just name,
    fsAttrs = [("class",classname)]
}


newCompanyForm :: UTCTime -> Form Company
newCompanyForm now = renderDivs $ Company
  <$> areq textField (defaultFormSettings "Title") Nothing
  <*> aopt textField (defaultFormSettings "Website") Nothing
  <*> aopt textField (defaultFormSettings "Image") Nothing
  <*> aopt textField (defaultFormSettings "Description") Nothing
  <*> areq textField (defaultFormSettings "Ticker") Nothing
  <*> aopt textField (defaultFormSettings "GICS Sector") Nothing
  <*> aopt textField (defaultFormSettings "GICS Sub Industry") Nothing
  <*> pure now
