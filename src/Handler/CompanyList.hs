module Handler.CompanyList where

import Import

getCompanyListR :: Handler Html
getCompanyListR = do
   companies <- runDB $ selectList [][Asc CompanyTitle]
   defaultLayout $ do
    setTitle "Investments info"
    toWidget [whamlet|
<section class="main special">
  <div class="content">
      <header>
        <h2>Companies</h2>
        <div>
          $forall Entity cid Company{..} <- companies
            <div>
              <h2>#{companyTitle}
              <a .link href=@{CompanyDetailsR cid}>view details
              <p>Ticker:
                <b>#{companyTicker}
              $maybe img <- companyImage
                <img src=#{img} width=100 />
              $nothing
                <img src=@{StaticR images_defaultimage_gif} width=100 />

              $maybe website <- companyWebsite
                  <p>Website:
                  <p>#{website}
              $maybe description <- companyDescription
                  <p>Description:
                  <p>#{description}

|]
