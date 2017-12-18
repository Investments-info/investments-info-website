{-# OPTIONS_GHC -Wno-unused-matches #-}
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
        <div .col-md-12 style="text-align:left">
          $forall Entity cid Company{..} <- companies
            <div .col-md-6 >
              <div .company-card>
                <h2>#{companyTitle}
                <p>Ticker:
                  <b>#{companyTicker}
                $maybe img <- companyImage
                  <!-- <img src=#{img} width=100 /> -->
                $nothing
                  <!-- <img src=@{StaticR images_defaultimage_gif} width=100 /> -->
                <p>
                  <a .btn .btn-sm .btn-primary .pull-right href=@{CompanyDetailsR cid}>view details
                <br />
        <div .clearfix>
|]
