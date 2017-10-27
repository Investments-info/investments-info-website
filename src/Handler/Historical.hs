module Handler.Historical where

import Import

getHistoricalR :: Handler Html
getHistoricalR = do
      defaultLayout $ do
        setTitle "Investments info"
        toWidget [whamlet|
<section id="intro" class="main">
    <div class="spotlight">
        <div class="content">
            <header class="major">
                <h2>Historical Graphs</h2>
|]
