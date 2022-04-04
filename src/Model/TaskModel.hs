module Model.TaskModel where

import Control.Lens
import Data.Text (Text)
import Data.Time
import Monomer

import qualified Types.Calendar as CALENDAR
import qualified Types.Task as TASK

data TaskModel = TaskModel {
  _tmTasks          :: Maybe [TASK.Task],
  _tmShowTasks      :: Bool,
  _tmTaskFilePath   :: FilePath,
  _tmShowDatePicker :: Bool,
  _tmTaskToUpdate   :: Int
  } deriving (Eq, Show)


makeLensesWith abbreviatedFields ''TaskModel

data TaskAction = AddTask   
                | UpdateDate Int
                | DeleteTask Int
                | EditTasks CALENDAR.Days
                | MoveTask Int CALENDAR.Days
                | CloseDatePicker
                | SaveTasks
                deriving (Eq, Show)

