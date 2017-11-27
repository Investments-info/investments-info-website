module Handler.LogViewer where

import Import

import System.Cmd
import System.Environment
import Unsafe.Coerce (unsafeCoerce)


getLogViewerR :: Handler Value
getLogViewerR = do
    logResult <- runConduitRes $ sourceFileBS "yahoo_.txt" .| mapC show .| unlinesC .| mapC reverse .| foldC
    return $ object
        ["logstring" .= logResult]
