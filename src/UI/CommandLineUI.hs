{-# LANGUAGE OverloadedStrings #-}

module UI.CommandLineUI where

import Control.Lens
import qualified Data.Text as T
import Monomer

-- | My Module
import Model

import qualified UI.FileBrowserUI as FileBrowserUI
import qualified UI.FileUI        as FileUI

-- Commandline is used to execute commands like opening
-- files or opening the settings.

commandLineUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
commandLineUI wenv model = zstack [box cl `nodeVisible` (model ^. commandLineModel . commandLineVisible)
                                  ,box errorBox `nodeVisible` (model ^. commandLineModel . showErrorLine)]
  where
    action = CommandLineEvent ExecuteCommandLine

    textField' = textField (commandLineModel . commandLine)
                 `nodeKey` "commandLine" 

    cl = keystroke [("Enter",action),("C-g",CommandLineEvent Executed)] textField'

    errorBox = keystroke [("Enter",abbortError),("C-g",abbortError)] errorTextField

    abbortError = CommandLineEvent AbortError

    errorTextField = textField (commandLineModel . errorLine)
                     `styleBasic` [textColor $ rgbHex "#ff2200"]
                     `nodeKey` "errorLine"


