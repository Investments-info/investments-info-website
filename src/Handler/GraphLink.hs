{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.GraphLink where

import Import

    

getGraphLinkR :: Handler Html
getGraphLinkR = 
    defaultLayout $ do
        $(widgetFile "graphs")
