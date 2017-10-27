{-# LANGUAGE RecordWildCards #-}
module Handler.CompanyDetails where

import Import

getCompanyDetailsR :: CompanyId -> Handler Html
getCompanyDetailsR cid = do
    Company {..} <- runDB $ get404 cid
    defaultLayout $ do
     setTitle "Investments info"
     toWidget [whamlet|
<section id="intro" class="main">
  <div class="container">
    <div class="content">
    <header class="major">
      <h2>Company Details : #{companyTitle}</h2>
      <ul id="tabs" class="nav nav-tabs" data-tabs="tabs">
        <li class="active">
           <a href="#company-info" data-toggle="tab">Company Info
        <li>
           <a href="#company-historical" data-toggle="tab">Historical Data

      <div id="my-tab-content" class="tab-content">
         <div class="tab-pane active" id="company-info">
            <h5>Company name: #{companyTitle}
            <h5>Company ticker: #{companyTicker}
            $maybe img <- companyImage
                 <p>
                   <img src="#{img}" />
            $maybe web <- companyWebsite
                 <p>website :
                    <a href=#{web}>#{web}

            $maybe desc <- companyDescription
                 <h5>Company Description:
                 <p>#{desc}

         <div class="tab-pane" id="company-historical">
            <h3>Historical Data
|]
     toWidget[julius|
<script type="text/javascript">
    jQuery(document).ready(function ($) {
        $('#tabs').tab();
    });
</script>
|]
