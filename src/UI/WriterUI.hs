module UI.WriterUI where

import Control.Lens
import qualified Data.Text as T
import Monomer

-- | My Module
import Model

import qualified UI.FileBrowserUI as FileBrowserUI
import qualified UI.FileUI        as FileUI

-- writerUI defines UI for FileBrowser an Writing area
writerUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
writerUI wenv model =
  hstack [ FileBrowserUI.fileBrowserUI wenv model
           `nodeVisible` (model ^. fileBrowserModel . showFileBrowser)
           -- nodeVisible determines wether or not a widget is visible
           -- using a bool from the model allows to show/hide by updating the bool
         , FileUI.fileUI wenv model
         ]
  `nodeVisible` (DisplayWriting == model ^. displayModel)


