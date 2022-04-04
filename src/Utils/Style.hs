{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Utils.Style where

import Monomer
    ( nodeKey,
      transparent,
      box,
      button,
      CmbBgColor(bgColor),
      CmbBorder(border),
      CmbPadding(padding),
      CmbRadius(radius),
      CmbStyleBasic(styleBasic),
      CmbStyleHover(styleHover),
      CmbTextCenter(textCenter),
      CmbTextColor(textColor),
      CmbTextFont(textFont),
      CmbTextLeft(textLeft),
      CmbTextMiddle(textMiddle),
      CmbTextRight(textRight),
      CmbTextSize(textSize),
      CmbWidth(width) )
-- | QuasiQuotestuff
import Data.Maybe ( fromJust )
import qualified Data.HashMap.Strict as DH
import Parser.QQ ( settingsQQ )
import Parser.Json ( JValue(JObject, JString) )
import Parser.Parser ()


-- Default Settings is loaded at startup
defaultSettings :: JValue
defaultSettings = do
  [settingsQQ|
{
  "tetris": {
    "normalColor": "#525152",
    "tetrisColor1": "#787878",
    "tetrisColor2": "#fcba03",
    "tetrisColor3": "#30fc03",
    "tetrisColor4": "#03fcc6",
    "tetrisColor5": "#db1fc8",
    "tetrisColor6": "#8d1fdb",
    "tetrisColor7": "#db1f38",
    "tetrisColor8": "#d5db1f",
    "tetrisTextColor": "#000000",
    "tetrisTextColor2": "#ffffff"
  },
  "fileBrowser": {
    "outerPaddingFB": "0",
    "innerPaddingFB": "0",
    "innerRadiusFB": "0",
    "innerPaddingFL": "0",
    "innerRadiusFL": "0",
    "fileBrowserFileLine": "#757575",
    "fileBrowserFolderLine": "#5f5f5f",
    "fileBrowserBackground": "#3c3939",
    "fileBrowserFolderLineHover": "#008bad",
    "fileBrowserFileLineHover": "#008bae",
    "fontColor": "#ffffff",
    "iconColor": "#ffffff",
    "fontColorIconFile": "#ffffff",
    "fontColorIconFolder": "#ffffff"
  },
  "taskColor": {
    "taskColor2": "#757575",
    "taskColor19": "#000000",
    "taskColor5": "#0c92cc",
    "taskColorDeleteBackground": "#91002e",
    "taskColorIndicatorLowHover": "#ffe100",
    "taskColorIndicatorMediumHover": "#e8942e",
    "taskColor8": "#41fa6f",
    "taskColor13": "#32a864",
    "taskColorIndicatorHigh": "#91002e",
    "taskColor17": "#9e9e9e",
    "taskColor15": "#448751",
    "taskColorIndicatorHighHover": "#f04360",
    "taskColor10": "#08d166",
    "taskColorIndicatorMedium": "#ff6600",
    "taskColor4": "#3d3d3d",
    "taskColor14": "#32663c",
    "taskColor6": "#ff2200",
    "taskColor12": "#000000",
    "taskColorIndicatorLow": "#e8ba56",
    "taskColor9": "#1fa15b",
    "taskColor18": "#d96c00",
    "taskColor3": "#808080",
    "taskColor16": "#b5b5b5",
    "taskColorDelete": "#f04360",
    "taskColor11": "#288afa",
    "taskColor7": "#fad241",
    "taskColor1": "#636363"
  },
  "settings": {
    "saveButtonUnsaved": "#91002e",
    "saveButtonSaved": "#288afa",
    "innerPadding": "0",
    "settingsColor3": "#1702cf",
    "settingsColor4": "#ffffff",
    "settingsColor1": "#090057",
    "outerPadding": "0",
    "innerRadius": "0",
    "saveButtonSavedHover": "#54f52f",
    "settingsColor2": "#10009e",
    "settingsColor5": "#000000",
    "saveButtonUnsavedHover": "#f04360"
  },
  "file": {
    "font": "Mono3",
    "tabColor": "#717162",
    "tabColorHover": "#7a7a7a",
    "saveButton": "#ffffff",
    "saveButtonHover": "#fad241",
    "activeIndicator": "#008bad",
    "inActiveIndicator": "#484848",
    "background": "#000000",
    "addButton": "#000000",
    "closeButton": "#000000",
    "closeButtonHover": "#f04360",
    "outerPaddingFileArea": "0",
    "paddingFileButtons": "0",
    "highLightColor": "#5b388c",
    "outerPaddingFileButtonH": "2",
    "outerPaddingFileButtonV": "0",
    "innerPaddingFileButton": "0",
    "paddingTabButtonH": "0",
    "paddingTabButtonV": "0",
    "radiusTabButton": "0",
    "lineHighLightColor": "#358770",
    "lineNumberBackground": "#358770",
    "textColor": "#ffffff",
    "backgroundColor": "#000000",
    "lineNumberColor": "#000000",
    "lineNumberHighlightColor": "#f04360",
    "textSize": "15"
  },
  "sidebar": {
    "normalItem": "#000000",
    "hoverItem": "#6fc975",
    "innerPadding": "0",
    "selectedItem": "#ffffff",
    "background": "#4a824e",
    "outerPadding": "0",
    "innerRadius": "0"
  },
  "haskell": {
    "KeywordTok": "#32a852",
    "PragmaTok": "#a83258",
    "SymbolTok": "#32a895",
    "VariableTok": "#0f67bf",
    "ConstructorTok": "#d98c11",
    "OperatorTok": "#700b68",
    "CharTok": "#e1e81c",
    "StringTok": "#e1e81c",
    "IntegerTok": "#ffffff",
    "RationalTok": "#ffffff",
    "CommentTok": "#e88be8",
    "SpaceTok": "#ffffff",
    "OtherTok": "#e8998b"
  }
}
|]


-- Lookup a value inside settings at position key1 key2
lookupUnsafe settings key key2 = do
  ((\(JString str) -> str)
   . fromJust
   . (DH.!? key2)
   . (\(JObject v) -> v)
   . fromJust
   . (DH.!? key)
   . (\(JObject v) -> v)) settings


-- Store value in settings a position key1 -> key2
storeUnsafe :: String -> String -> String -> JValue -> JValue
storeUnsafe colorString key1 key2 settings = do
  let category = (\(JObject v) -> v) $ fromJust $ (DH.!? key1) $ (\(JObject v) -> v) settings
  let category' = DH.singleton key1 $ JObject $ DH.update (\a -> Just (JString colorString)) key2 category
  JObject $ DH.union category' ((\(JObject v) -> v) settings)


-- helper to be used for styling of textIcons (Remix ttf)
styleRemix color = [padding 5
                   , textFont "Remix"
                   , textSize 20
                   , textRight
                   , textMiddle
                   , textColor color
                   , bgColor transparent
                   , border 0 transparent]


-- Helper to pack UI Widget into double box
-- Inner box - background color
-- Outer box - adds empty frame around inner box
boxArea content innerPadding innerRadius innerBackgroundColor outerPadding =
  box ( box ( content)
        `styleBasic` [padding innerPadding
                     , radius innerRadius
                     , bgColor innerBackgroundColor])
  `styleBasic` [padding outerPadding]

-- helper for icon with icon text (Remix ttf)
remixButton icon action textAction textColor' key =
  button icon action
  `styleBasic`[textFont "Remix"
              , width 50
              , textSize 30
              , textLeft
              , textColor textAction
              , textCenter
              , textMiddle
              , bgColor transparent
              , border 0 transparent]
  `styleHover` [textColor textColor']
  `nodeKey` key
