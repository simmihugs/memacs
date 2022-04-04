{-# LANGUAGE OverloadedStrings #-}
module UI.Calendar.Calendar where


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

-- calendar displays in month view
-- allows to jump to prev/next month or Year and back to
-- current day
calendar :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
calendar wenv model = do
  vstack [ filler
         , hstack [filler,vstack [header, scroll $ calendar'''],filler]
         , filler]
  where
    calendar''' = box ( box calendar'''' `styleBasic` [padding 5, radius 15, bgColor $ rgbHex taskColor2]) `styleBasic` [padding 5]

    header :: WidgetNode AppModel AppEvent
    header = box (box (hstack [ button "today" (CalendarEvent $ UpdateDayToDisplay (model ^. calendarModel . today))
                                -- Jump to current day
                              , button remixArrowLeftSLine (CalendarEvent $ UpdateDayToDisplay previousMonth)
                                `styleBasic` Style.styleRemix (rgbHex taskColor16)
                                `styleHover` [textColor $ rgbHex taskColor5]
                                -- Jump to previous month
                              , textDropdownSV month
                                (\x -> CalendarEvent $ UpdateDayToDisplay $ CALENDAR.dateToDays (weekday, day, x, year))
                                (map CALENDAR.Month [1..12]) `styleBasic` [width 80]
                                -- Select Month from dropdown
                              , button remixArrowRightSLine (CalendarEvent $ UpdateDayToDisplay  nextMonth)
                                `styleBasic` Style.styleRemix (rgbHex taskColor16)
                                `styleHover` [textColor $ rgbHex taskColor5]
                                -- Jump to next Month
                              , spacer
                              , button remixArrowLeftSLine (CalendarEvent $ UpdateDayToDisplay  previousYear)
                                `styleBasic` Style.styleRemix (rgbHex taskColor16)
                                `styleHover` [textColor $ rgbHex taskColor5]
                                -- Jump to previous year
                              , textDropdownSV year
                                (\x -> CalendarEvent $ UpdateDayToDisplay $ CALENDAR.dateToDays (weekday, day, month,x))
                                (map CALENDAR.Year [1900..2030]) `styleBasic` [width 80]
                                -- Select Year from dropdown
                              , button remixArrowRightSLine (CalendarEvent $ UpdateDayToDisplay  nextYear)
                                `styleBasic` Style.styleRemix (rgbHex taskColor16)
                                `styleHover` [textColor $ rgbHex taskColor5]
                                -- Jump to next year
                              ])
                          `styleBasic` [radius 5, padding 5, bgColor $ rgbHex taskColor2])
                     `styleBasic` [padding 5]

    previousYear = CALENDAR.dateToDays (weekday, day, month, pred' year)
    nextYear     = CALENDAR.dateToDays (weekday, day, month, succ' year)
    pred' (CALENDAR.Year year) = CALENDAR.Year $ pred year
    succ' (CALENDAR.Year year) = CALENDAR.Year $ succ year
    pred'' (CALENDAR.Month month) = CALENDAR.Month $ pred month
    succ'' (CALENDAR.Month month) = CALENDAR.Month $ succ month
    previousMonth
      | month==CALENDAR.Month 1 = CALENDAR.dateToDays (weekday, day, CALENDAR.Month 12, pred' year)
      | otherwise               = CALENDAR.dateToDays (weekday, day, pred'' month, year)
    nextMonth
      | month==CALENDAR.Month 12 = CALENDAR.dateToDays (weekday, day, CALENDAR.Month 1, succ' year)
      | otherwise                = CALENDAR.dateToDays (weekday, day, succ'' month, year)

    -- | get all relevant colors
    taskColor1 = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColor1"
    taskColor2 = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColor2"
    taskColor3 = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColor3"
    taskColor4 = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColor4"
    taskColor5 = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColor5"
    taskColor6 = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColor6"
    taskColor7 = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColor7"
    taskColor8 = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColor8"
    taskColor9 = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColor9"
    taskColor10 = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColor10"
    taskColor11 = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColor11"
    taskColor12 = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColor12"
    taskColor13 = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColor13"
    taskColor14 = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColor14"
    taskColor15 = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColor15"
    taskColor16 = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColor16"
    taskColor17 = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColor17"
    taskColor18 = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColor18"
    taskColor19 = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColor19"
    taskColorIndicatorLowHover    = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColorIndicatorLowHover"
    taskColorIndicatorLow         = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColorIndicatorLow"
    taskColorIndicatorMediumHover = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColorIndicatorMediumHover"
    taskColorIndicatorMedium      = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColorIndicatorMedium"
    taskColorIndicatorHighHover   = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColorIndicatorHighHover"
    taskColorIndicatorHigh        = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColorIndicatorHigh"
    taskColorDelete               = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColorDelete"
    taskColorDeleteBackground     = Style.lookupUnsafe (model ^. settingsModel . settings) "taskColor" "taskColorDeleteBackground"


    calendar'''' = vstack $ hstack (startingOffset ++ weekdays) : map rows month'
    
    today' = CALENDAR.calculateDate (model ^. calendarModel . today)
    dayToDisplay'@(weekday, day, month, year) = CALENDAR.calculateDate (model ^. calendarModel . dayToDisplay)

    -- | Different Color if day is current day, if day is in current month (or not)
    normalDayColor selectedDay@(w,d,m,y)
      | today'==selectedDay = rgbHex taskColor11
      | m==month            = rgbHex taskColor1
      | otherwise           = rgbHex taskColor17  
    hoverDayColor selectedDay@(w,d,m,y)
      | today'==selectedDay = rgbHex taskColor5
      | m==month            = rgbHex taskColor3
      | otherwise           = rgbHex taskColor16
    month' = CALENDAR.createMonth year month
  
    startingOffset = [label "" `styleBasic` [width 65]]
    weekdays       = intersperse spacer $
                     map (\x -> label x `styleBasic` [width 100])
                     ["MO","TU","WE","TH","FR","SA","SU"]
  
    rows (weeknumber, week') =
      hstack $ (box (label $ pack $ show weeknumber)
                 `styleBasic` [width 20]) : makeWeek week'

    makeWeek Nothing     = []
    makeWeek (Just week') = map makeDay week'

    -- | TaskIndicators
    -- | Show up to 5 task indicators sorted by priority
    -- | an is done or not
    taskIndicators date = case model ^. taskModel . tasks of
      Nothing -> vstack []
      Just tasks -> hstack $ (\x -> [filler]++x++[filler]) $ map makeIndicator $ take 5 $ sort $ TASK.filterTasks date $ TASK.makeIndexed tasks 
        where
          makeIndicator (TASK.TaskIndexed (index,TASK.Task title' text' date' priority' done'))
            | done'                    = greenBox
            | priority' == TASK.High   = prio1Box
            | priority' == TASK.Medium = prio2Box
            | otherwise                = prio3Box
          greenBox = box (box (label "")
                           `styleBasic` [radius 2, width 10, height 10, border 1 $ rgbHex taskColor19, bgColor $ rgbHex taskColor14]
                           `styleHover` [bgColor $ rgbHex taskColor13]) `styleBasic` [padding 2]
          prio1Box = box (box (label "")
                          `styleBasic` [radius 2, width 10, height 10, border 1 $ rgbHex taskColor19, bgColor $ rgbHex taskColorIndicatorHigh]
                          `styleHover` [bgColor $ rgbHex taskColorIndicatorHighHover]) `styleBasic` [padding 2]
          prio2Box = box (box (label "")
                          `styleBasic` [radius 2, width 10, height 10, border 1 $ rgbHex "#000000", bgColor $ rgbHex taskColorIndicatorMedium]
                          `styleHover` [bgColor $ rgbHex taskColorIndicatorMediumHover]) `styleBasic` [padding 2]
          prio3Box = box (box (label "")
                          `styleBasic` [radius 2, width 10, height 10, border 1 $ rgbHex "#000000", bgColor $ rgbHex taskColorIndicatorLow]
                          `styleHover` [bgColor $ rgbHex taskColorIndicatorLowHover]) `styleBasic` [padding 2]

    -- | Make the box representing a day
    makeDay selectedDay@(w,d,m,y) = box_ [onClick $ TaskEvent $ EditTasks $ CALENDAR.dateToDays selectedDay] day' `styleBasic` [padding 5]
      where
        day' =  box (vstack [hstack [filler,label $ pack $ show d,filler]
                            , spacer
                            , taskIndicators $ CALENDAR.dateToDays selectedDay])
                `styleBasic` [width 100
                             , height 100
                             , radius 10
                             , bgColor (normalDayColor selectedDay)]
                `styleHover` [bgColor (hoverDayColor selectedDay)]

