{-# LANGUAGE OverloadedStrings #-}
module Events.CommandLineEvents where

import Monomer
import Control.Lens
import Data.Text ( unpack, pack )
import qualified Graphics.UI.TinyFileDialogs as Tiny
import qualified System.FilePath as F


-- | My Modules
import qualified Events.Writer.WriterEvents      as WriterEV
import qualified Events.Writer.FileBrowserEvents as FileBrowserEV
import qualified Events.Writer.FileEvents        as FileEV
import qualified Events.Calendar.CalendarEvents  as CalendarEV
import qualified Events.Calendar.TaskEvents      as TaskEV
import qualified Events.TetrisEvents             as TetrisEV
import qualified Events.SettingsEvents           as SettingsEV
import qualified Utils.Tools                     as Tools
import Model
import Parser.Parser
import Parser.CommandLine


handleCommandLineEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> CommandLineAction
  -> [AppEventResponse AppModel AppEvent]
handleCommandLineEvent wenv node model evt = case evt of
  -- | Open commandline on bottom
  ShowCommandLine      -> [Model $ model & commandLineModel . commandLineVisible .~ True
                          , SetFocusOnKey "commandLine"]
  -- | close
  Executed             -> [Model $ model
                           & commandLineModel . commandLineVisible .~ False
                           & commandLineModel . commandLine .~ ""]

  -- | Open Settings
  ShowSettings         -> [Model $ model
                          & commandLineModel . commandLineVisible .~ False
                          & commandLineModel . commandLine .~ ""
                          & displayModel .~ DisplaySettings]

  -- | Open File
  CommandOpenFile file -> [Task $ return $ TetrisEvent TetrisStop
                          , Model $ model
                            & displayModel .~ DisplayWriting
                            & commandLineModel . commandLineVisible .~ False
                            & commandLineModel . commandLine .~ ""
                          , Task $ return $ FileEvent $ OpenFile file
                          , SetFocusOnKey "textArea"]

  -- | Start displaying Error
  ShowError command    -> [Model $ model
                           & commandLineModel . errorLine .~ command
                           & commandLineModel . showErrorLine .~ True
                          , SetFocusOnKey "errorLine"]

  -- | Stop displaying Error
  AbortError          -> [Model $ model
                           & commandLineModel . errorLine .~ ""
                           & commandLineModel . commandLine .~ ""
                           & commandLineModel . showErrorLine .~ False
                          , SetFocusOnKey "commandLine"]

  -- | Create new file
  NewFile -> [Task $ newFile]
    where
      newFile = do
        let filePath = (model ^. fileBrowserModel . currentDirectory) F.</> "newfile.tmp"
        return $ CommandLineEvent (CommandOpenFile (False,filePath,""))

  -- | Save file under new name
  SaveAs fileName -> [Task $ saveAs fileName]
    where
      saveAs fileName = do
        case model ^. fileModel . files of
          Nothing -> return $ CommandLineEvent (ShowError $ "No open file")
          _       -> do
            let index = model ^. fileModel . indexCurrentFile
            let (_,filePath,fileContent) = model ^. getFileI index
            if F.takeDirectory fileName == "."
              then do
              let newFileName = (model ^. fileBrowserModel . currentDirectory) F.</> fileName
              (success,message) <- Tools.tryWriteFile newFileName fileContent
              if success
                then return $ FileEvent (UpdateName index newFileName)
                else return $ AppendErrorMessage (pack message)
              else do
              (success,message) <- Tools.tryWriteFile fileName fileContent
              if success
                then return $ FileEvent (UpdateName index fileName)
                else return $ AppendErrorMessage (pack message)

  -- | Try to Open a file
  TryOpenFile fileName -> [Task $ tryOpenFile fileName]
    where
      tryOpenFile fileName = do
        result <- Tools.tryOpenFile fileName
        case result of
          (False,_,_) -> do
            let newDir = (model ^. fileBrowserModel . currentDirectory) F.</> fileName
            result2 <- Tools.tryOpenFile newDir
            case result2 of
              (False,_,_) -> return $ CommandLineEvent (ShowError $ pack $ "Could not open file: " ++ fileName)
              file        -> return $ CommandLineEvent (CommandOpenFile file)
          file -> return $ CommandLineEvent (CommandOpenFile file)

  -- | Try to open FileOpenDialog
  TryOpenFileDialog -> [Task $ tryOpenFileDialog]
    where
      tryOpenFileDialog = do
        result <- Tiny.openFileDialog "Open file." (pack $ model ^. fileBrowserModel . currentDirectory) [] "" False
        case result of
          Nothing             -> return EmptyEvent
          Just maybeFilePaths -> case maybeFilePaths of
            []            -> return $ CommandLineEvent (ShowError "No filePath returned.")
            (filePath:xs) -> do
              text <- Tools.tryOpenFile (unpack filePath)
              case text of
                (False,_,_) -> return $ CommandLineEvent (ShowError $ "Could not open file: " <> filePath)
                file        -> return $ CommandLineEvent (CommandOpenFile file)

  -- | Execute Commandline
  ExecuteCommandLine -> [Task $ executeCommand]
    where
      executeCommand = do
        let commandStr = model ^. commandLineModel . commandLine
        let command = run cl (unpack commandStr) 
        case command of
          Nothing -> return $ CommandLineEvent (ShowError "Invalid command.")
          Just (INS action,_) -> case action of
            ("settings",_)        -> return $ CommandLineEvent ShowSettings
            ("newFile","")        -> return $ CommandLineEvent NewFile
            ("openFile","")       -> return $ CommandLineEvent TryOpenFileDialog
            ("openFile",fileName) -> return $ CommandLineEvent (TryOpenFile fileName)
            ("saveAs",fileName)   -> return $ CommandLineEvent (SaveAs fileName)
            _                     -> return $ CommandLineEvent
                                 (ShowError $ "Somehow got here! : " <> commandStr)
            
