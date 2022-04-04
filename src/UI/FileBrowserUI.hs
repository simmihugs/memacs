{-# LANGUAGE OverloadedStrings #-}
module UI.FileBrowserUI where

import Control.Lens
import Monomer
import Data.List
import Data.Text ( pack, Text )

-- | My modules
import qualified Types.Calendar as CALENDAR
import qualified Types.Task as TASK
import qualified Utils.Style as Style
import Model


-- helper type to make sorting for fileBrowser
newtype FileBrowserLine = FileBrowserLine (Bool, FileType, FilePath) deriving (Eq, Show)
instance Ord FileBrowserLine where
  (FileBrowserLine (l1,t1,p1)) `compare` (FileBrowserLine (l2,t2,p2)) = compare' l1 l2
    where
      compare' True  False = GT
      compare' False True  = LT
      compare' _     _     = compare'' p1 p2
      compare'' p1 p2
        | '.' `elem` p1 && not ('.' `elem` p2) = GT
        | not ('.' `elem` p1) && '.' `elem` p2 = LT
        | otherwise                            = compare''' t1 t2
      compare''' TypeFile TypeDirectory = LT
      compare''' TypeDirectory TypeFile = GT
      compare''' _             _        = EQ


-- fileBrowserUI allows to browse the filesystem an open files
fileBrowserUI :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
fileBrowserUI wenv model = box bar `styleBasic` [padding outerPaddingFB] `nodeKey` "fileBrowser"
  where
    -- | Get all relevant colors from settings
    outerPaddingFB = read $ Style.lookupUnsafe (model ^. settingsModel . settings) "fileBrowser" "outerPaddingFB" 
    innerPaddingFB = read $ Style.lookupUnsafe (model ^. settingsModel . settings) "fileBrowser" "innerPaddingFB" 
    innerRadiusFB = read $ Style.lookupUnsafe (model ^. settingsModel . settings) "fileBrowser" "innerRadiusFB" 
    innerPaddingFL = read $ Style.lookupUnsafe (model ^. settingsModel . settings) "fileBrowser" "innerPaddingFL" 
    innerRadiusFL = read $ Style.lookupUnsafe (model ^. settingsModel . settings) "fileBrowser" "innerRadiusFL" 

    fileBrowserFileLine =  Style.lookupUnsafe (model ^. settingsModel . settings) "fileBrowser" "fileBrowserFileLine" 
    fileBrowserFolderLine =  Style.lookupUnsafe (model ^. settingsModel . settings) "fileBrowser" "fileBrowserFolderLine" 
    fileBrowserBackground =  Style.lookupUnsafe (model ^. settingsModel . settings) "fileBrowser" "fileBrowserBackground" 
    fileBrowserFolderLineHover =  Style.lookupUnsafe (model ^. settingsModel . settings) "fileBrowser" "fileBrowserFolderLineHover" 
    fileBrowserFileLineHover =  Style.lookupUnsafe (model ^. settingsModel . settings) "fileBrowser" "fileBrowserFileLineHover" 

    iconColor =  Style.lookupUnsafe (model ^. settingsModel . settings) "fileBrowser" "iconColor" 
    fontColor =  Style.lookupUnsafe (model ^. settingsModel . settings) "fileBrowser" "fontColor" 
    fontColorIconFile =  Style.lookupUnsafe (model ^. settingsModel . settings) "fileBrowser" "fontColorIconFile" 
    fontColorIconFolder =  Style.lookupUnsafe (model ^. settingsModel . settings) "fileBrowser" "fontColorIconFolder" 

    bar = box files' `styleBasic` [padding innerPaddingFB
                                  , width 300
                                  , radius innerRadiusFB
                                  , bgColor $ rgbHex fileBrowserBackground]
    files' = vstack [ header
                    , scroll_ [wheelRate 50]
                      (vstack $ (\x -> x ++ [filler]) $ map fileBrowserLine $ sort
                        $ map FileBrowserLine
                        $ filterHiddenLocked (model ^. fileBrowserModel . dirContent))
                    , filler]

    -- | the header is on top of the filebrowser
    header = box (hstack [ button remixFileAddLine (CommandLineEvent NewFile)
                           `styleBasic` Style.styleRemix (rgbHex fontColor)
                           `styleHover` [textColor $ rgbHex fileBrowserFolderLineHover]
                         , button remixFolderOpenLine (FileBrowserEvent OpenDirectoryDialog)
                           `styleBasic` Style.styleRemix (rgbHex fontColor)
                           `styleHover` [textColor $ rgbHex fileBrowserFolderLineHover]
                         , button remixHome2Line (FileBrowserEvent GoHome)
                           `styleBasic` Style.styleRemix (rgbHex fontColor)
                           `styleHover` [textColor $ rgbHex fileBrowserFolderLineHover]
                         , button remixArrowUpLine (FileBrowserEvent Updir)
                           `styleBasic` Style.styleRemix (rgbHex fontColor )
                           `styleHover` [textColor $ rgbHex fileBrowserFolderLineHover]
                         , scroll (label (pack (model ^. fileBrowserModel . currentDirectory))
                                    `styleBasic` [textColor $ rgbHex fontColor]
                                    `styleHover` [textColor $ rgbHex fileBrowserFolderLineHover])
                         , filler ]) `styleBasic` [height 30]

    -- | Filters hidden and or locked files out of filebrowser
    filterHiddenLocked 
      | not (model ^. fileBrowserModel . showHidden) && not (model ^. fileBrowserModel . showLocked) = filterHidden . filterLocked
      | not (model ^. fileBrowserModel . showHidden)                                                 = filterHidden
      | not (model ^. fileBrowserModel . showLocked)                                                 = filterLocked
      | otherwise                                                                                    = id
    filterHidden = filter (\(_,_,p) -> not (null p) && (head p /= '.'))
    filterLocked = filter (\(x,_,_) -> not x) 


    -- | assignes different color to files and folders
    fileOrDirectoryColor TypeFile filepath = fileColor filepath
    fileOrDirectoryColor _        filepath = directoryColor 
    fileOrDirectoryColorHover TypeFile filepath = fileColorHover filepath
    fileOrDirectoryColorHover _        filepath = directoryColorHover

    directoryColor      = rgbHex fileBrowserFolderLine 
    directoryColorHover = rgbHex fileBrowserFolderLineHover

    fileColor      f = rgbHex fileBrowserFileLine
    fileColorHover f = rgbHex fileBrowserFileLineHover 

    -- creates the actual file/folder line
    fileBrowserLine (FileBrowserLine (lock, filetype, filepath)) = 
      box_ [onClick $ FileBrowserEvent $ OpenFileOrDir (filetype, filepath)] x
      `styleBasic` [padding 2]
      where
        lineIcon -- which icon to display (Folder or File or lock)
          | lock               = remixLock2Line
          | filetype/=TypeFile = remixFolder2Line
          | otherwise          = remixFile2Line 

        x = box (hstack [ label lineIcon `styleBasic` Style.styleRemix (rgbHex iconColor)
                        , label (pack $ (\x -> if length x>27
                                               then take 24 x ++ "..."
                                               else x) filepath)
                          `styleBasic` [textColor $ if filetype==TypeFile
                                                    then rgbHex fontColorIconFile
                                                    else rgbHex fontColorIconFolder]
                        , filler])
            `styleBasic` [padding innerPaddingFL
                         , radius innerRadiusFL
                         , width 280
                         , bgColor $ fileOrDirectoryColor filetype filepath]
            `styleHover` [bgColor  $ fileOrDirectoryColorHover filetype filepath]

          



