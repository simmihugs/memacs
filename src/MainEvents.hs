{-# LANGUAGE OverloadedStrings #-}
module MainEvents where

import Monomer
import Control.Lens
import Data.Text ( unpack )

-- | My Modules
import qualified Events.Writer.WriterEvents      as WriterEV
import qualified Events.Writer.FileBrowserEvents as FileBrowserEV
import qualified Events.Writer.FileEvents        as FileEV
import qualified Events.Calendar.CalendarEvents  as CalendarEV
import qualified Events.Calendar.TaskEvents      as TaskEV
import qualified Events.TetrisEvents             as TetrisEV
import qualified Events.SettingsEvents           as SettingsEV
import qualified Events.CommandLineEvents        as CommandLineEV
import qualified Utils.Tools                     as Tools
import Model


-- Handle Events by passing different Events to their
-- dedicated EventHandlers located in `Events`.
handleEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> AppEvent
  -> [AppEventResponse AppModel AppEvent]
handleEvent wenv node model evt = case evt of
  EmptyEvent                 -> []
  TetrisEvent x              -> TetrisEV.handleTetrisEvent wenv node model x
  WriteEvent x               -> WriterEV.handleWriteEvent wenv node model x
  FileBrowserEvent x         -> FileBrowserEV.handleFileBrowserEvent wenv node model x
  FileEvent x                -> FileEV.handleFileEvent wenv node model x
  CalendarEvent x            -> CalendarEV.handleCalendarEvent wenv node model x
  TaskEvent x                -> TaskEV.handleTaskEvent wenv node model x
  SettingsEvent x            -> SettingsEV.handleSettingsEvent wenv node model x
  CommandLineEvent x         -> CommandLineEV.handleCommandLineEvent wenv node model x
  -- In case AppendErrorMessage happens
  -- 2 things happen
  -- The model is updated
  -- A Task is executed. Tasks in Monomer are separed threads which can perform
  -- IO, but have to return an Event aka. cannot run forever
  AppendErrorMessage message -> [Model $ model & errorModel . errorLog %~ (<> message)
                                , Task $ Tools.writeLog (model ^. errorModel . errorLogFilePath)
                                  (unpack message)
                                  >> return EmptyEvent]

