{-# LANGUAGE OverloadedStrings #-}

module UI.SideBarUI where

import Control.Lens
import qualified Data.Text as T
import Monomer


-- | My Module
import Model
import qualified Utils.Style as Style


sideBarUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
sideBarUI wenv model =
  -- Wrap list of buttons in vertical stack
  boxArea' $ vstack [fileButton, calendarButton, startButton, codeButton, filler, settingsButton]
  where
    -- Define Buttons which on Click raise AppEvent.
    -- Buttons use Icon from Remix ttf (Comes with gui library)
    boxArea' content = Style.boxArea content iP iR (rgbHex background) oP

    settingsButton = Style.remixButton remixSettings3Fill action textAction (rgbHex hoverItem) "sButton"
      where
        action = SettingsEvent ShowIt
        textAction = if model ^. displayModel == DisplaySettings
                     then rgbHex selectedItem
                     else rgbHex normalItem

    startButton = Style.remixButton remixGamepadLine action textAction (rgbHex hoverItem) "startButton"
      where
        action = if model ^. displayModel == DisplayTetris
                 then TetrisEvent TetrisEnd
                 else TetrisEvent TetrisStart
        textAction = if model ^. displayModel == DisplayTetris
                     then rgbHex selectedItem
                     else rgbHex normalItem

    calendarButton = Style.remixButton remixCalendarLine action textAction (rgbHex hoverItem) "calendarButton"
      where
        action = CalendarEvent ShowCalendar
        textAction = if model ^. displayModel == DisplayCalendar
                     then rgbHex selectedItem
                     else rgbHex normalItem

    fileButton = Style.remixButton remixFile2Line action textAction (rgbHex hoverItem) "fileButton"
      where
        action = if model ^. displayModel == DisplayWriting                 
                 then FileBrowserEvent ShowFileFileBrowser 
                 else FileBrowserEvent OpenTextArea
        textAction = if model ^. displayModel == DisplayWriting
                        && model ^. fileBrowserModel . showFileBrowser
                     then rgbHex selectedItem
                     else rgbHex normalItem
    codeButton = Style.remixButton remixCodeSSlashLine action textAction (rgbHex hoverItem) "codeButton"
      where action = if model ^. displayModel == DisplayWriting
                     then WriteEvent StopWriting
                     else WriteEvent StartWriting
            textAction = if model ^. displayModel == DisplayWriting
                         then rgbHex selectedItem
                         else rgbHex normalItem

    -- Lookup Colors from settings
    selectedItem = Style.lookupUnsafe (model ^. settingsModel . settings) "sidebar" "selectedItem"
    normalItem   = Style.lookupUnsafe (model ^. settingsModel . settings) "sidebar" "normalItem"
    background   = Style.lookupUnsafe (model ^. settingsModel . settings) "sidebar" "background"
    hoverItem    = Style.lookupUnsafe (model ^. settingsModel . settings) "sidebar" "hoverItem"    
    iP           = read $ Style.lookupUnsafe (model ^. settingsModel . settings) "sidebar" "innerPadding"
    iR           = read $ Style.lookupUnsafe (model ^. settingsModel . settings) "sidebar" "innerRadius"
    oP           = read $ Style.lookupUnsafe (model ^. settingsModel . settings) "sidebar" "outerPadding"

