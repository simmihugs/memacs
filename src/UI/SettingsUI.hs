{-# LANGUAGE OverloadedStrings #-}
module UI.SettingsUI where

import Control.Lens
import Monomer
import Data.List
import Data.Text ( pack, Text )
import qualified Data.Text as T
import Data.Maybe
import qualified System.FilePath as F
import qualified Data.HashMap.Strict as DH
import qualified Monomer.Lens as L
import Data.Typeable
import Data.Char

-- | My modules
import qualified Types.Calendar as CALENDAR
import qualified Types.Task as TASK
import qualified Utils.Style as Style
import Parser.Json
import Parser.Hex
import Model

-- This is needed for the Fontpicker dropdown
-- The provided widget expects a type which inplements
-- Show or TextShow -- If one uses strings the result
-- In the dropdown are inside ""
newtype FontSymbol = FontSymbol String deriving (Eq, Typeable)
instance Show FontSymbol where
  show (FontSymbol s) = s
unSymbol (FontSymbol s) = s

settingsUI :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
settingsUI wenv model =
  boxArea' (vstack [ hstack [ createUI, filler] , filler])
  `nodeVisible` (DisplaySettings == model ^. displayModel)
  where
    boxArea' content = Style.boxArea content iP iR iBC oP

    -- Get relevant colors from settings
    iBC = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "settings" "settingsColor1"
    iBC2 = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "settings" "settingsColor2"
    iBC3 = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "settings" "settingsColor3"
    tc  = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "settings" "settingsColor4"
    tc2  = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "settings" "settingsColor5"
    iP  = read $ Style.lookupUnsafe (model ^. settingsModel . settings) "settings" "innerPadding"
    iR  = read $ Style.lookupUnsafe (model ^. settingsModel . settings) "settings" "innerRadius"
    oP  = read $ Style.lookupUnsafe (model ^. settingsModel . settings) "settings" "outerPadding"
    sBU  = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "settings" "saveButtonUnsaved"
    sBS    = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "settings" "saveButtonSaved"
    sBUH = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "settings" "saveButtonUnsavedHover"
    sBSH   = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "settings" "saveButtonSavedHover"

    -- Button to Save changed Settings
    saveButton = Style.remixButton remixSave2Line (SettingsEvent SaveSettings) tC' tCHover' "saveSettings"
      where
        tC' = if model ^. settingsModel . unsaved -- check if changes to settings have happend
              then sBU
              else sBS
        tCHover' = if model ^. settingsModel . unsaved -- check if changes to settings have happend
                   then sBUH
                   else sBSH

    -- The indicator allows to change Values which are Color strings, Numbers or Fonts
    indicator key key2 string
      | isColorHex string                = colorIndicator
      | foldl1 (&&) (map isDigit string) = numberIndicator
      | otherwise                        = fontIndicator
      where
        -- for Colorstring
        colorIndicator = hstack [box_ [onClick $ SettingsEvent (ColorPicker key key2)]
                                 ( box ( hstack [ label "" ])
                                   `styleBasic` [width 20
                                                , height 20
                                                , bgColor $ rgbHex string
                                                , border 1 (if (unpack $ fst $ fromJust $ run hex string) > 50000 then tc2 else tc)]
                                   `styleHover`[bgColor $ (rgbHex string) & L.a .~ 0.8])
                                 `styleBasic` [padding 5]
                                , box ( label "" ) `styleBasic` [width 80]]

        -- for Numbers
        numberIndicator = hstack [box (box ( textDropdownSV (read string) (\a -> SettingsEvent (StoreColor (show a) key key2)) ([0..20] :: [Int]) )
                                       `styleBasic` [width 90])
                                  `styleBasic` [paddingV 5]
                                 , box ( label " " ) `styleBasic` [width 20]]

        -- for Fonts
        fontIndicator = hstack [box (box ( textDropdownSV (FontSymbol string) (\a -> SettingsEvent $ UpdateFont (unSymbol a) key key2) (map (FontSymbol . T.unpack) $ model ^. settingsModel . fonts))
                                     `styleBasic` [width 90])
                                `styleBasic` [paddingV 5]
                               , box ( label " " ) `styleBasic` [width 20]]


    -- The actual uI creation
    -- maps over the settings and creates a list of Widgets
    -- which hold lists of Wigets representing all settings items.
    createUI = do
      let a = map (\(key,value) -> (key,(DH.toList $ (\(JObject x) -> x) $ value))) $ DH.toList $ (\(JObject x) -> x) (model ^. settingsModel . settings)
      vstack [ box(box (hstack [filler, saveButton])
               `styleBasic` [padding 10, radius 10, bgColor iBC3])
               `styleBasic` [padding 5]
             , scroll_ [wheelRate 50] $ vstack $
               map (\(key,value) -> box (box $ vstack [ box (box (hstack [label (pack key),filler])
                                                             `styleBasic` [padding 10, radius 10])
                                                        `styleBasic` [padding 5]
                                                      , ((vstack $ map (\(key2,value2) -> (box (hstack [ indicator key key2 (((\(JString x) -> x) value2))
                                                                                                         , spacer
                                                                                                         , label (pack key2), filler])
                                                                                              `styleBasic` [padding 10
                                                                                                           , radius 10])
                                                                                          `styleBasic` [padding 5]) value))
                                                        `styleBasic` [bgColor iBC3
                                                                     , padding 10
                                                                     , radius 10]])
                                    `styleBasic` [padding 5]) a]
  
          
