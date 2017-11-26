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
              <div style="padding:20px;border:1px solid #ebebeb;margin-top:20px">
                <h2>#{companyTitle}
                <p>Ticker:
                  <b>#{companyTicker}
                $maybe img <- companyImage
                  <img src=#{img} width=100 />
                $nothing
                  <img src=@{StaticR images_defaultimage_gif} width=100 />
                <p>
                  <a .btn .btn-sm .btn-default .pull-right href=@{CompanyDetailsR cid}>view details
        <div .clearfix>
|]
