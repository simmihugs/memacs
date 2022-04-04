{-# LANGUAGE OverloadedStrings #-}
module Events.Writer.FileBrowserEvents where

import Control.Lens
import Control.Exception
import Monomer
import Data.List
import Data.Text ( unpack, pack, Text )
import qualified System.FilePath as F
import qualified Graphics.UI.TinyFileDialogs as Tiny
import Control.Exception (SomeException(SomeException))


-- | My modules
import qualified Types.Calendar as CALENDAR
import qualified Types.Task as TASK
import qualified Utils.Tools as Tools
import qualified Utils.FileBrowser as FileBrowser
import qualified Utils.Style as Style
import Model


-- | Open directory opening dialog
openDirectoryDialog model = do
  x <- Tiny.selectFolderDialog "" (pack $ model ^. fileBrowserModel . currentDirectory)
  case x of
    Nothing       -> return EmptyEvent
    Just filePath -> do
      let filePath' = (\x -> (FileBrowser.isDir x,x)) $ unpack filePath
      return $ FileBrowserEvent (OpenFileOrDir filePath')

-- | Open Home directory
goHome model = do
  let homeDir = model ^. fileBrowserModel . homeDirectory
  let homeDir' = (\x -> (FileBrowser.isDir x,x)) homeDir
  return $ FileBrowserEvent (OpenFileOrDir homeDir')


-- | Navigate one directory up
updir model filePath = do
  newFiles <- FileBrowser.listFiles filePath 
  return $ FileBrowserEvent (Storedir filePath newFiles)


handleFileBrowserEvent :: WidgetEnv AppModel AppEvent
                       -> WidgetNode AppModel AppEvent
                       -> AppModel
                       -> FileBrowserAction
                       -> [AppEventResponse AppModel AppEvent]
handleFileBrowserEvent wenv node model evt = case evt of
  -- | Show hidden files in filebrowser
  ShowHidden          -> [Model $ model & fileBrowserModel . showHidden %~ not
                         , Task $ return $ AppendErrorMessage "FILEBROWSER_ACTION: change show hidden files.\n"
                         ]

  -- | Show locked files in filebrowser
  ShowLocked          -> [Model $ model & fileBrowserModel . showLocked %~ not
                         , Task $ return $ AppendErrorMessage "FILEBROWSER_ACTION: change show locked files.\n"
                         ]

  -- | open the directory dialog
  OpenDirectoryDialog -> [Task $ openDirectoryDialog model
                         , Task $ return $ AppendErrorMessage "FILEBROWSER_ACTION: started open directory dialog.\n"
                         ]

  -- | navigate to home directory
  GoHome -> [Model $ model & fileBrowserModel . currentDirectory .~
              (model ^. fileBrowserModel . homeDirectory)
            , Task $ goHome model
            , Task $ return $ AppendErrorMessage "FILEBROWSER_ACTION: reverted to homedir.\n"
            ]

  -- | use as new directory to display in file browser
  Storedir filepath newFiles -> [Model $ model
                                  & fileBrowserModel . currentDirectory .~ filepath
                                  & fileBrowserModel . dirContent .~ newFiles
                                , Task $ return $ AppendErrorMessage "FILEBROWSER_ACTION: Dir stored in model.\n"
                                ]

  -- | move up in the direcotry hierachie
  Updir -> [Model $ model & fileBrowserModel . currentDirectory .~ newDir
           , Task $ updir model newDir
           , Task $ return $ AppendErrorMessage "FILEBROWSER_ACTION: Switched to parent dir.\n"
           ]           
    where
      newDir = F.takeDirectory (model ^. fileBrowserModel . currentDirectory)

  -- | try opening a file or directory by click in the filebrowser
  OpenFileOrDir (TypeDirectory,filePath) -> do
    --1. calculate new dir
    let newDir = (model ^. fileBrowserModel . currentDirectory) F.</> filePath
    --2. test If dir is locked (no permission)
    [Task $ testDir newDir]
      where
        testDir newDir = do
          result <- try (FileBrowser.listFiles newDir) :: IO (Either SomeException [(Bool, FileType, FilePath)])
          case result of
            Left ex -> do
              let message = pack $ "ERROR: Could not open dir: " ++ show ex ++ "\n"
              return $ AppendErrorMessage message
            Right newFiles' -> do
              return $ FileBrowserEvent (Storedir newDir newFiles')
  OpenFileOrDir (_,filePath) ->  [Task $ openFile' filePath
                                 , Task $ return $ AppendErrorMessage "FILEBROWSER_ACTION: Opened file.\n"
                                 ]           
    where
      openFile' filePath = do
        file' <- Tools.tryOpenFile ((model ^. fileBrowserModel . currentDirectory) F.</> filePath )
        return $ FileEvent (OpenFile file')

  -- | hide/displaying the filebrowser
  ShowFileFileBrowser -> [Model $ model & fileBrowserModel . showFileBrowser %~ not]

  -- | hide/displaying the textArea
  OpenTextArea        -> [Model $ model & fileBrowserModel . showFileBrowser .~ True
                         , Task $ return $ WriteEvent StartWriting]

        
