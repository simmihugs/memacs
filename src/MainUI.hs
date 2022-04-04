{-# LANGUAGE OverloadedStrings #-}
module MainUI where

import Control.Lens
import qualified Data.Text as T
import Monomer

-- | My Module
import qualified UI.WriterUI      as WriterUI
import qualified UI.SideBarUI     as SideBarUI
import qualified UI.TetrisUI      as TetrisUI
import qualified UI.CalendarUI    as CalendarUI
import qualified UI.SettingsUI    as SettingsUI
import qualified UI.CommandLineUI as CommandLineUI
import Model

-- mainUI builds the ui from the commponents found in UI
-- It is wrapped in a keystroke which raises an event 
mainUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
mainUI wenv model = widgetTree where
  widgetTree = keystroke [("Alt-x", CommandLineEvent ShowCommandLine)] (
    hstack [ SideBarUI.sideBarUI wenv model
           , presentArea `nodeKey` "textArea"])

  -- zstack allows to display widgets on top of each other
  -- The top layer has to be in visble for the bottom on to
  -- be accesible. In this instance all members of the zstack
  -- are mutually exclusive with each other.
  presentArea = vstack [zstack [ TetrisUI.tetrisUI wenv model
                               , WriterUI.writerUI wenv model
                               , CalendarUI.calendarUI wenv model
                               , SettingsUI.settingsUI wenv model]
                       , filler
                       , CommandLineUI.commandLineUI wenv model
                       ]
