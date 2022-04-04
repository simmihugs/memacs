{-# LANGUAGE OverloadedStrings #-}
module UI.TetrisUI where

import Control.Lens
import qualified Data.Text as T
import Monomer

-- | My Module
import qualified Widget.TetrisWidget as TetrisWidget
import qualified Utils.Style as Style
import Model

-- tetrisUI defines UI for the TetrisGame
-- This function is kinda a hack
-- The tetris ui does not visually update without user input
-- internally it does update, if it receives a user input then
-- the game key jumps down. In order to cope this widget uses
-- a time label which continously displays the current time
-- which triggers the tetris ui to redraw - but to not see
-- a clock, the color of the clock is set to the background
-- color.
tetrisUI
  :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
tetrisUI wenv model = vstack [
  hstack [ timeLabel `nodeKey` "fadeTimeLabel" -- Is needed for the gui to update 
         , spacer
         , label (T.pack $ (show $ model ^. tetrisModel . randomIndex))  -- Originally for debugging
           `styleBasic` [textColor $ rgbHex normalColor]
           `nodeKey` "indexLabel"]

    -- The actual tetris widget. Its wrapped in a keystroke widget.
    -- On `Esc` the Event `TetrisEvent TetrisEnd` is raised
    -- See Events/TetrisEvents.hs for more details.
  , keystroke [("Esc",TetrisEvent TetrisEnd)]
    (TetrisWidget.tetris model `nodeKey` "tetris")
  ]
  `nodeVisible` (DisplayTetris == model ^. displayModel)
  where
    timeString = T.pack . show $ model ^. tetrisModel . timeTicker
    
    timeLabel = label timeString
                `styleBasic` [textColor $ rgbHex normalColor -- background color so timelabel is invisble
                             , textFont "Bold"
                             , textSize 25
                             , textCenter
                             , textMiddle
                             , padding 10]

    normalColor = Style.lookupUnsafe (model ^. settingsModel . settings) "tetris" "normalColor"
