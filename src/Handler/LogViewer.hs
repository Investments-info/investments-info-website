module Handler.LogViewer where

import           Import


getLogViewerR :: Handler Value
getLogViewerR = do
    _ <- requireAdmin
    logResult <- runConduitRes $ sourceFileBS "yahoo_.txt" .| mapC show .| unlinesC .| mapC reverse .| foldC
    return $ object
        ["log" .= logResult]
