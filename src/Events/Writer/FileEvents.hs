{-# LANGUAGE OverloadedStrings #-}
module Events.Writer.FileEvents where

import Control.Lens
import Monomer
import Data.List
import Data.Text ( pack, Text)
import Data.Maybe
import qualified System.FilePath as F


-- | My modules
import qualified Types.Calendar as CALENDAR
import qualified Types.Task as TASK
import qualified Utils.Style as Style
import qualified Utils.Tools as Tools
import Model



handleFileEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> FileAction
  -> [AppEventResponse AppModel AppEvent]
handleFileEvent wenv node model x = case x of
  -- | Mark file as changed
  SetChanged index   -> [Model $ model & isChangedI index .~ True] 

  -- | Mark file as unchanged
  SetUnChanged index message -> [Model $ model & isChangedI index .~ False
                                , Task $ return $ AppendErrorMessage
                                  $ pack ("FILE_ACTION: Set File with index "
                                          ++ show index ++ " unchanged.\n")
                                ]  

  -- | update name of file with index
  UpdateName index fileName -> [Model $ model & isChangedI index .~ False
                                 & isChangedI index .~ False
                                 & getFilePathI index .~ fileName                                 
                                 & commandLineModel . commandLineVisible .~ False
                                 & commandLineModel . commandLine .~ ""
                               , SetFocusOnKey "textArea"
                               , Task $ return $ FileBrowserEvent
                                 (OpenFileOrDir (TypeDirectory,F.takeDirectory fileName))
                               , Task $ return $ AppendErrorMessage
                                 $ pack ("FILE_ACTION: Update file with index "
                                          ++ show index ++ " to " ++ fileName ++ ".\n")
                               ]  
  -- | Openfile
  OpenFile file'@(bool, filepath, text) -> do
    let currentFiles = model ^. fileModel . files
    case currentFiles of
      Nothing        -> [Model $ model
                          & fileModel . files ?~ [file']
                          & displayModel .~ DisplayWriting
                        , SetFocusOnKey "textArea"
                        , Task $ return $ AppendErrorMessage
                          $ pack ("FILE_ACTION: File added: " ++ filepath ++ "\n")
                        ]
      (Just files'') -> do
        let (bool',filePath',text') = file'
        if not $ any (\(_,filePath'',_) -> filePath''==filePath') files''
          then [Model $ model
                & fileModel . files ?~ (files'' ++ [file' & _1 .~ False ])
                & displayModel .~ DisplayWriting
                & fileModel . indexCurrentFile .~ length (model ^. fileModel . files . _Just)
               , SetFocusOnKey "textArea"
               , Task $ return $ AppendErrorMessage $ "FILE_ACTION: File added: " <> pack filePath' <> ".\n"
               ]
          else [Task $ return $ AppendErrorMessage "FILE_ERROR: File could not be opened.\n"]

  -- | Close file without saving it
  CloseWithoutSaving index' -> do
    let currentFiles = model ^. fileModel . files
    case currentFiles of
      Nothing        -> [Task $ return $ AppendErrorMessage "FILE_ERROR: No files available to be closed.\n"]
      Just [oneFile] -> [Model $ model & fileModel . files .~ Nothing
                        , Task $ return $ AppendErrorMessage "FILE_ACTION: Last file closed.\n"
                        ]
      Just files'     -> closeFile files' index'
        where
          closeFile files' index'
            | index' == 0 = [Model $ model & fileModel . files ?~ tail files'
                            , Task $ return $ AppendErrorMessage "FILE_ACTION: First file closed.\n"
                            ]
            | index' == pred (length files') =
              [Model $ model
                & fileModel . files ?~ init files'
                & fileModel . indexCurrentFile .~ pred (pred $ length files')
              , Task $ return $ AppendErrorMessage "DEBUG: You reached me\n"
              ]
            | otherwise = [Model $ model
                           & fileModel . files ?~ (take index' files' ++ drop (succ index') files')
                          , Task $ return $ AppendErrorMessage "FILE_ACTION: One file closed.\n"
                          ]

  -- | Save file with index
  SaveFile index          -> [Task $ saveFile index
                             , Task $ return $ AppendErrorMessage "FILE_ACTION: File was saved.\n"
                             ]
    where
      saveFile index = do
        let (_,filePath,fileContent) = model ^. getFileI index
        (success,message) <- Tools.tryWriteFile filePath fileContent
        if success
          then return $ FileEvent (SetUnChanged index message)
          else return $ AppendErrorMessage (pack message)

  -- | Put focus on file with index
  ShowFile index'    -> [Model $ model
                          & fileModel . indexCurrentFile .~ index'
                        , Task $ return $ FileBrowserEvent (OpenFileOrDir (TypeDirectory,filePath'))
                        , Task $ return $ AppendErrorMessage $ "FILE_ACTION: File with index: "
                          <> pack (show index') <> " is displayed now.\n"
                        ]
    where
      filePath' = F.takeDirectory (model ^. getFilePathI index')

