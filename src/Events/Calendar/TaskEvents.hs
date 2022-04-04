{-# LANGUAGE OverloadedStrings #-}
module Events.Calendar.TaskEvents where

import Control.Lens
import Monomer
import Data.List
import Data.Text ( pack, Text )
import Control.Exception
import Data.Default

-- | My modules
import qualified Types.Calendar as CALENDAR
import qualified Types.Task as TASK
import qualified Utils.Tools as Tools
import qualified Utils.Style as Style
import Model

handleTaskEvent :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> TaskAction
  -> [AppEventResponse AppModel AppEvent]
handleTaskEvent wenv node model evt = case evt of
  -- | Save changes to task and stop displaying tasks
  SaveTasks       -> [Model $ model & taskModel . showTasks .~ False
                      , Task writeTasks ]
    where
      message = "SUCCESS: closed taskview\n"
      errorMessage = "ERROR: Could not write tasks to file.\n"
      successMessage = "SUCCESS: Wrote tasks to file.\n"
      warningMessage = "WARNING: Empty tasks cannot be written"
      writeTasks = do
        result <- try (TASK.writeTasksMaybe
                       (model ^. taskModel . taskFilePath) (model ^. taskModel . tasks)) :: IO (Either SomeException Bool)
        case result of
          Left ex       -> return $ AppendErrorMessage (pack errorMessage)
          Right bool -> if bool
                        then return $ AppendErrorMessage (pack successMessage)
                        else return $ AppendErrorMessage (pack warningMessage)

  -- | Start displaying tasks
  EditTasks days   -> [Model $ model
                        & taskModel . showTasks .~ True
                        & calendarModel . dayOfTask .~ days
                      , Task $ return $ AppendErrorMessage $ pack message
                      ]
    where
      message = "SUCCESS: Opened tasks for editing\n"

  -- | Add Task
  AddTask          -> [Model $ model & taskModel . tasks %~ addTask
                      , Task $ return $ AppendErrorMessage $ pack message
                      ]
    where
      message = "SUCCESS: added task\n"
      addTask Nothing      = Just [def & TASK.date .~ (model ^. calendarModel . dayOfTask)]
      addTask (Just tasks) = Just ((def & TASK.date .~ (model ^. calendarModel . dayOfTask)) : tasks)
  MoveTask index date -> [Model $ model
                           & dateOfTaskI index .~ date
                           & taskModel . showDatePicker .~ False]

  -- | Show Datepicker for date with index 
  UpdateDate index -> [Model $ model
                        & taskModel . showDatePicker .~ True
                        & taskModel . taskToUpdate .~ index]

  -- | Stop showing datepicker
  CloseDatePicker  -> [Model $ model
                       & taskModel . showDatePicker .~ False
                      , Task $ return $ CalendarEvent (UpdateDayToDisplay dateToUpdate)]
    where
      dateToUpdate = model ^. dateOfTaskI (model ^. taskModel . taskToUpdate)

  -- | Delete task with index 
  DeleteTask index -> [Model $ model & taskModel . tasks %~ deleteTask index
                      , Task $ return $ AppendErrorMessage $
                        pack ("SUCCESS: deleted task with index: " ++ show index ++ "\n")
                      ]
    where
      deleteTask index Nothing = Nothing
      deleteTask index (Just tasks)
        | 1==length tasks = Nothing
        | otherwise       = deleteTask' index tasks
      deleteTask' index tasks
        | index==0                   = Just $ tail tasks
        | index==pred (length tasks) = Just $ init tasks
        | otherwise                  = Just $ take index tasks ++ drop (succ index) tasks


