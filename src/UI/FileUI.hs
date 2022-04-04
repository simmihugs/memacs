{-# LANGUAGE OverloadedStrings #-}
module UI.FileUI where

import Control.Lens
import Monomer
import Data.List
import qualified Data.List as List
import Data.Text ( pack, Text, isInfixOf )
import Data.Maybe
import qualified System.FilePath as F

-- | My modules
import qualified Types.Calendar as CALENDAR
import qualified Types.Task as TASK
import qualified Utils.Style as Style
import Model

import GHC.SyntaxHighlighter -- For syntax tokens

import qualified MyTextArea as My
import qualified MyCombinators as Cmb

textChange2 :: Int -> Text -> AppEvent
textChange2 index text = FileEvent (SetChanged index)

fileUI :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
fileUI wenv model = case model ^. fileModel . files of
  Nothing -> vstack []
  _       -> do
    let index      = model ^. fileModel . indexCurrentFile
    let filePath'  = model ^. getFilePathI index

    let isHaskell  = ".hs" `List.isInfixOf` filePath'
    let syntaxTree = tokenizeHaskellLoc (model ^. getTextI index)
    let syntaxMap = [(KeywordTok    ,rgbHex "#03dffc")
                    , (PragmaTok     ,rgbHex "#ffb638")
                    , (SymbolTok     ,rgbHex "#e1e81c")
                    , (VariableTok   ,rgbHex "#ffffff")
                    , (ConstructorTok,rgbHex "#03fc24")    
                    , (OperatorTok   ,rgbHex "#e1e81c")
                    , (CharTok       ,rgbHex "#ff2200")
                    , (StringTok     ,rgbHex "#ff2200")
                    , (IntegerTok    ,rgbHex "#ffffff")
                    , (RationalTok   ,rgbHex "#ffffff")
                    , (CommentTok    ,rgbHex "#fc9003")
                    , (SpaceTok      ,rgbHex "#ffffff")
                    , (OtherTok      ,rgbHex "#ff38e8")]


    vstack [ hstack [fileButtons, filler]
           , My.textArea_ (getTextI index)
             (Cmb.showLineNumbers_ (model ^. fileModel . showLineNumbers)
               : [wheelRate 50, acceptTab]
               ++ [Cmb.lineNumberBackgroundColor lineNumberBackground]
               ++ [Cmb.lineNumberNumberColor lineNumberColor]
               ++ [Cmb.lineNumberNumberHighlightColor lineNumberHighlightColor]
               ++ [Cmb.currentLineColor lineHighLightColor | model ^. fileModel . highlightCurrentLine]
               ++ [Cmb.syntax (syntaxTree,syntaxMap) | model ^. fileModel . fShowSyntax])
             `styleBasic` [bgColor backgroundColor
                          , textColor textColor'
                          , textFont $ Font $ pack font
                          , textSize fTextSize
                          , hlColor lineHighLightColor]
           ]
      where
        -- | Get all necessary colors
        lookupTetris x = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "tetris" x
        keyWord'     = lookupTetris "KeywordTok"
        pragma'      = lookupTetris "PragmaTok"
        symbol'      = lookupTetris "SymbolTok"
        variable'    = lookupTetris "VariableTok"
        constructor' = lookupTetris "ConstructorTok"
        operator'    = lookupTetris "OperatorTok"
        char'        = lookupTetris "CharTok"
        string'      = lookupTetris "StringTok"
        integer'     = lookupTetris "IntegerTok"
        rational'    = lookupTetris "RationalTok"
        comment'     = lookupTetris "CommentTok"
        space'       = lookupTetris "SpaceTok"
        other'       = lookupTetris "OtherTok"

        backgroundColor = rgbHex $ lookupFile "backgroundColor"
        textColor' = rgbHex $ lookupFile "textColor"
        lineNumberBackground = rgbHex $ lookupFile "lineNumberBackground"
        lineNumberColor = rgbHex $ lookupFile "lineNumberColor"
        lineNumberHighlightColor = rgbHex $ lookupFile "lineNumberHighlightColor"
        lineHighLightColor = rgbHex $ lookupFile "lineHighLightColor"
        highLightColor = rgbHex $ lookupFile "highLightColor"
        lookupFile = Style.lookupUnsafe (model ^. settingsModel . settings) "file"
        background = rgbHex $ lookupFile "background"

        fTextSize :: Double
        fTextSize = read $ lookupFile "textSize"

        outerPaddingFileArea :: Int
        outerPaddingFileArea = read $ lookupFile "outerPaddingFileArea"

        paddingFileButtons = read $ lookupFile "paddingFileButtons"
        outerPaddingFileButtonH = read $ lookupFile "outerPaddingFileButtonH"
        outerPaddingFileButtonV = read $ lookupFile "outerPaddingFileButtonV"

        innerPaddingFileButton :: Int
        innerPaddingFileButton = read $ lookupFile "innerPaddingFileButton"

        radiusTabButton = read $ lookupFile "radiusTabButton"

        paddingTabButtonH = read $ lookupFile "paddingTabButtonH"
        paddingTabButtonV = read $ lookupFile "paddingTabButtonV"

        activeIndicatorColor = rgbHex $ lookupFile "activeIndicator"
        inActiveIndicatorColor = rgbHex $ lookupFile "inActiveIndicator"

        closeButtonColor = rgbHex $ lookupFile "closeButton"
        closeButtonColorHover = rgbHex $ lookupFile "closeButtonHover"

        font = lookupFile "font"

        addButtonC = rgbHex $ lookupFile "addButton"

        tabColor = rgbHex $ lookupFile "tabColor"
        tabColorHover = rgbHex $ lookupFile "tabColorHover"

        saveButtonC = rgbHex $ lookupFile "saveButton"
        saveButtonHoverC = rgbHex $ lookupFile "saveButtonHover" 

        -- | The Line of Tabs representing files and Plus button at the end of the tabs for new file 
        fileButtons = box (hstack $ map fileButtons' [0..pred $ length $ fromJust (model ^. fileModel . files)]
                            ++ [newFileButton])
                    `styleBasic` [padding paddingFileButtons]
        -- | CreateNew File
        newFileButton = button remixAddLine (CommandLineEvent NewFile)
                        `styleBasic` Style.styleRemix addButtonC

        -- | Box around tab (for padding)
        -- | Which onclick set focus on tab/file
        fileButtons' index =
          box_ [onClick $ FileEvent (ShowFile index)] innerBox 
          `styleBasic` [paddingH outerPaddingFileButtonH
                       ,paddingV outerPaddingFileButtonV]
          where
            isChanged = model ^. isChangedI index
            innerBox = vstack [ tab , activeIndicator ]
  
            -- | The actual Tab
            tab = box ( hstack [ saveButton
                               , title'
                               , closeButton])
                    `styleBasic` [paddingV paddingTabButtonV
                                 , paddingH paddingTabButtonH
                                 , radius radiusTabButton
                                 , bgColor tabColor]
                    `styleHover` [bgColor tabColorHover]

            -- | Indicates if a tab is active
            activeIndicator = box ( vstack [filler,label "",filler] )
                              `styleBasic`
                              [height 5, bgColor $ if index == (model ^. fileModel . indexCurrentFile)
                                                   then activeIndicatorColor
                                                   else inActiveIndicatorColor ]

            -- | Button for saving the file
            saveButton = button remixSave2Line (if isChanged then FileEvent (SaveFile index) else EmptyEvent)
                         `styleBasic` Style.styleRemix (if isChanged
                                                        then saveButtonC 
                                                        else transparent)
                         `styleHover` [textColor (if isChanged
                                                   then saveButtonHoverC
                                                   else transparent)]

            -- | Button to close Tab/File
            closeButton = if isChanged then killButton' else closeButton'
              where
                closeButton' = button remixCloseLine (FileEvent $ CloseWithoutSaving index)
                               `styleBasic` Style.styleRemix closeButtonColor
                               `styleHover` [textColor closeButtonColorHover]
                killButton'  = button remixSkull2Line (FileEvent $ CloseWithoutSaving index)
                               `styleBasic` Style.styleRemix closeButtonColor
                               `styleHover` [textColor closeButtonColorHover]
            title'      = label (pack $ F.takeFileName $ model ^. getFilePathI index)
    
