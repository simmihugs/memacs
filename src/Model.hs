{-
The module Model is the central data representation of the
monomer app. It allows to access and update (in event
handler) the data representation of the app.

Model imports forwards all model submodules.
-}
module Model
  (
    module Model,
    module Model.ErrorModel,
    module Model.FileBrowserModel,
    module Model.FileModel,
    module Model.CalendarModel,
    module Model.TaskModel,
    module Model.SettingsModel,
    module Model.TetrisModel,
    module Model.ChannelModel,
    module Model.CommandLineModel
  )
where 

import Control.Lens
import Data.Text (Text)
import Control.Concurrent.Chan
import Control.Concurrent.STM.TChan

-- | 
import Model.ErrorModel
import Model.FileBrowserModel
import Model.FileModel
import Model.CalendarModel
import Model.TaskModel
import Model.SettingsModel
import Model.TetrisModel
import Model.ChannelModel
import Model.CommandLineModel

import qualified Types.Calendar as CALENDAR
import qualified Types.Task as TASK


-- DisplayMode is used to display one or none ui element in
-- the main presentArea of the app.
data DisplayMode = DisplayNone
                 | DisplayTetris
                 | DisplayWriting
                 | DisplayCalendar
                 | DisplaySettings
                 deriving (Eq, Show)


-- For operations where the current DisplayMode is compared
-- to a potential new DisplayMode. If both are the same
-- then dont display; if their are not, display the new one.
showOrHide :: DisplayMode -> DisplayMode -> DisplayMode
showOrHide x y
  | x==y      = DisplayNone
  | otherwise = x


data AppModel = AppModel {
  _amErrorModel         :: ErrorModel,
  _amSettingsModel      :: SettingsModel,
  _amCalendarModel      :: CalendarModel,
  _amTaskModel          :: TaskModel,
  _amTetrisModel        :: TetrisModel,
  _amDisplayModel       :: DisplayMode,
  _amFileBrowserModel   :: FileBrowserModel,
  _amFileModel          :: FileModel,
  _amChannelModel       :: ChannelModel,
  _amCommandLineModel   :: CommandLineModel
  } deriving (Eq, Show)


-- lenses for AppModel where the lens is formed and
-- its name does not have abreviated fields
-- e.g. _amCommandLineModel --> commandLineModel
makeLensesWith abbreviatedFields ''AppModel


-- Bundles all Events, which can be raced by UI function.
-- By handling events the Model can be update and the app
-- will be rerendered.
data AppEvent = EmptyEvent
              | TaskEvent TaskAction
              | CalendarEvent CalendarAction
              | TetrisEvent TetrisAction
              | WriteEvent WriteAction
              | FileBrowserEvent FileBrowserAction
              | FileEvent FileAction
              | AppendErrorMessage Text
              | SettingsEvent SettingsAction
              | CommandLineEvent CommandLineAction
              deriving (Eq, Show)


-- | FileModel lenses
-- | For more conveniece lenses to directly access members
-- | deep inside fileModel.
getFileI i = lens (getter i) (setter i)
  where
    getter i m     = m ^?! fileModel . files . _Just . element i
    setter i m nab = m &   fileModel . files . _Just . element i .~ nab

getTextI i = lens (getter i) (setter i)
  where
    getter i m     = m ^?! fileModel . files . _Just . element i . _3
    setter i m nab = m &   fileModel . files . _Just . element i . _3 .~ nab

getFilePathI i = lens (getter i) (setter i)
  where
    getter i m     = m ^?! fileModel . files . _Just . element i . _2
    setter i m nab = m &   fileModel . files . _Just . element i . _2 .~ nab

isChangedI i = lens (getter i) (setter i)
  where
    getter i m     = m ^?! fileModel . files . _Just . element i . _1
    setter i m nab = m &   fileModel . files . _Just . element i . _1 .~ nab

-- | TaskModel lenses
-- | For more conveniece lenses to directly access members
-- | deep inside taskModel.
isDoneI i = lens (getter i) (setter i)
  where
    getter i m     = m ^?! taskModel . tasks . _Just . element i . TASK.done
    setter i m nab = m &   taskModel . tasks . _Just . element i . TASK.done .~ nab

isTitleI i = lens (getter i) (setter i)
  where
    getter i m     = m ^?! taskModel . tasks . _Just . element i . TASK.title
    setter i m nab = m &   taskModel . tasks . _Just . element i . TASK.title .~ nab

isPriorityI i = lens (getter i) (setter i)
  where
    getter i m     = m ^?! taskModel . tasks . _Just . element i . TASK.priority
    setter i m nab = m &   taskModel . tasks . _Just . element i . TASK.priority .~ nab

dateOfTaskI i = lens (getter i) (setter i)
  where
    getter i m     = m ^?! taskModel . tasks . _Just . element i . TASK.date
    setter i m nab = m &   taskModel . tasks . _Just . element i . TASK.date .~ nab

