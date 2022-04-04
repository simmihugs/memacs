{-# LANGUAGE OverloadedStrings #-}
module UI.CalendarUI where


import Control.Lens
import Monomer
import qualified Monomer.Lens as L
import Data.List
import Data.Text ( pack, Text )


-- | My modules
import qualified Types.Calendar as CALENDAR
import qualified Types.Task as TASK
import qualified Utils.Style as Style
import Model

import qualified UI.Calendar.Calendar as Calendar
import qualified UI.Calendar.DatePicker as DatePicker
import qualified UI.Calendar.Task as Task

-- calendarUI displays a calendar. On each day
-- There can be tasks, which are done or are still todos
-- by clicking on a day, the tasks of this day will be
-- shown. Tasks can be modified and or delete

calendarUI :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
calendarUI wenv model = zstack [ Calendar.calendar wenv model `nodeVisible` not (model ^. taskModel . showTasks)
                               , Task.task wenv model `nodeVisible` model ^. taskModel . showTasks
                               , DatePicker.datePicker wenv model `nodeVisible` model ^. taskModel . showDatePicker
                               ] `nodeVisible` (DisplayCalendar == model ^. displayModel)
