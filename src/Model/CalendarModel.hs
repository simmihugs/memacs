module Model.CalendarModel where

import Control.Lens
import Data.Text (Text)
import Data.Time
import Monomer

import qualified Types.Calendar as CALENDAR
import qualified Types.Task as TASK

data CalendarModel = CalendarModel {
  _cmToday        :: CALENDAR.Days,
  _cmDayToDisplay :: CALENDAR.Days,
  _cmDayOfTask    :: CALENDAR.Days
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''CalendarModel

data CalendarAction = UpdateDayToDisplay CALENDAR.Days
                    | ShowCalendar
                    deriving (Eq, Show)

