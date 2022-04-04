{-# LANGUAGE OverloadedStrings #-}
module UI.Calendar.DatePicker where


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


-- | Allows to move Task to different day
datePicker :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
datePicker wenv model = vstack [filler, hstack [filler, datePicker', filler], filler ]
                        `styleBasic` [bgColor (transparent & L.a .~ 0.8)]
  where
    -- | Get all relevant colors
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
  
    today' = CALENDAR.calculateDate (model ^. calendarModel . today)
    dayToDisplay'@(weekday, day, month, year) = CALENDAR.calculateDate (model ^. calendarModel . dayToDisplay)

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


    -- Display Month 
    datePicker' = vstack [closeDatePicker, box ( box calendar''' `styleBasic` [padding 5, radius 15, bgColor $ rgbHex taskColor2]) `styleBasic` [padding 5]]
      where
        normalDayColor selectedDay@(w,d,m,y)
          | today'==selectedDay = rgbHex taskColor11
          | m==month            = rgbHex taskColor1
          | otherwise           = rgbHex taskColor17  
        hoverDayColor selectedDay@(w,d,m,y)
          | today'==selectedDay = rgbHex taskColor5
          | m==month            = rgbHex taskColor3
          | otherwise           = rgbHex taskColor16
        calendar''' = vstack (weekdays : map rows month')
      


        month' = CALENDAR.createMonth year month
        -- Add Weekdays on top of calendar
        weekdays       = hstack ((label "" `styleBasic` [width 20])
                                 :(intersperse spacer $
                                   map (\x -> label x `styleBasic` [width 30])
                                   ["MO","TU","WE","TH","FR","SA","SU"]))
    
        rows (weeknumber, week') =
          hstack $ (box (label $ pack $ show weeknumber)
                    `styleBasic` [width 20]) : makeWeek week'

        -- | Buttons to move prev/next month/year and close date picker
        closeDatePicker = Style.boxArea (hstack [taskButton
                                                , previousMonthButton
                                                , monthDropdown
                                                , nextMonthButton
                                                , previousYearButton
                                                , yearDropdown
                                                , nextYearButton
                                                , filler
                                                , closeDatePicker']) 5 5 (rgbHex taskColor2) 10
                          `styleBasic` [width 500]
          where
            dateOfCurrentTask   = model ^. dateOfTaskI (model ^. taskModel . taskToUpdate)
            taskButton          = button "task" (CalendarEvent $ UpdateDayToDisplay dateOfCurrentTask)
            closeDatePicker'    = Style.remixButton remixCloseLine       (TaskEvent CloseDatePicker)                        (rgbHex taskColor16) (rgbHex taskColor6) "closeDatePicker"
            previousMonthButton = Style.remixButton remixArrowLeftSLine  (CalendarEvent $ UpdateDayToDisplay previousMonth) (rgbHex taskColor16) (rgbHex taskColor5) "pmb"
            nextMonthButton     = Style.remixButton remixArrowRightSLine (CalendarEvent $ UpdateDayToDisplay  nextMonth)    (rgbHex taskColor16) (rgbHex taskColor5) "nmb"
            previousYearButton  = Style.remixButton remixArrowLeftSLine  (CalendarEvent $ UpdateDayToDisplay  previousYear) (rgbHex taskColor16) (rgbHex taskColor5) "pyb"
            nextYearButton      = Style.remixButton remixArrowRightSLine (CalendarEvent $ UpdateDayToDisplay  nextYear)     (rgbHex taskColor16) (rgbHex taskColor5) "nyb"

            monthDropdown =  textDropdownSV month
                             (\x -> CalendarEvent $ UpdateDayToDisplay $ CALENDAR.dateToDays (weekday, day, x, year))
                             (map CALENDAR.Month [1..12]) `styleBasic` [width 80]
            yearDropdown = textDropdownSV year
                           (\x -> CalendarEvent $ UpdateDayToDisplay $ CALENDAR.dateToDays (weekday, day, month, x))
                           (map CALENDAR.Year [1900..2030]) `styleBasic` [width 80]


  
        -- create a List of Day boxes
        makeWeek Nothing     = []
        makeWeek (Just week') = map makeDay week'
 
        -- Create Box for a day
        makeDay selectedDay@(w,d,m,y) = box_ [onClick $ TaskEvent (MoveTask (model ^. taskModel . taskToUpdate) (CALENDAR.dateToDays selectedDay))] day' `styleBasic` [padding 5]
          where
            day' =  box (hstack [filler,label $ pack $ show d,filler])
                    `styleBasic` [width 30
                                 , height 30
                                 , radius 10
                                 , bgColor (normalDayColor selectedDay)]
                    `styleHover` [bgColor (hoverDayColor selectedDay)]




