{-# LANGUAGE OverloadedStrings #-}
module Events.Calendar.CalendarEvents where

import Control.Lens
import Monomer
import Data.List
import Data.Text ( pack, Text )

-- | My modules
import qualified Types.Calendar as CALENDAR
import qualified Types.Task as TASK
import qualified Utils.Tools as Tools
import qualified Utils.Style as Style
import Events.TetrisEvents
import Model


handleCalendarEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> CalendarAction
  -> [AppEventResponse AppModel AppEvent]
handleCalendarEvent wenv node model x = case x of
  -- | Starts displaying Calendar
  ShowCalendar            -> [Task $ return $ TetrisEvent TetrisStop
                             , Model $ model & displayModel %~ showOrHide DisplayCalendar
                             , Task $ return $ AppendErrorMessage
                               $ pack ("CALENDAR_ACTION: Show Calendar\n")]

  -- | Change day to be displayed
  UpdateDayToDisplay days -> [
    Model $ model & calendarModel . dayToDisplay .~ days
    , Task $ return $ AppendErrorMessage "CALENDAR_ACTION: Updated Day to display.\n"]

