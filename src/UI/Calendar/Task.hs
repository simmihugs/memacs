{-# LANGUAGE OverloadedStrings #-}
module UI.Calendar.Task where


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


task :: WidgetEnv AppModel AppEvent
  -> AppModel
  -> WidgetNode AppModel AppEvent
task wenv model = do
  vstack [ box (box (hstack [box (hstack [label "Done", filler]) `styleBasic` [width 100]
                  , spacer
                  , box (hstack [label "Priority", filler]) `styleBasic` [width 100]
                  , spacer
                  , box (hstack [label "title", filler]) `styleBasic` [width 100]
                  , filler
                  , saveTasks]
               ) `styleBasic` [radius 10
                              , padding 10
                              , bgColor $ rgbHex taskColor3]
               ) `styleBasic` [padding 5]
         , listOfTasks
         , filler
         , hstack [addButton, filler]
         ] 
  where
    -- | Get all relevant colors from settings
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

    -- | return to calendar
    saveTasks = button remixSave2Line (TaskEvent SaveTasks)
                `styleBasic` Style.styleRemix (rgbHex taskColor17)
                `styleHover` [textColor $ rgbHex taskColor9]
    addButton = box addButton' `styleBasic` [padding 5]
      where
        addButton'  = box addButton''
                      `styleBasic`
                      [bgColor (rgbHex taskColor11), radius 30, width 60, height 60]
        addButton'' = button remixAddLine (TaskEvent AddTask) `styleBasic` Style.styleRemix (rgbHex taskColor7)

    -- | List of Tasks
    listOfTasks = scroll $ vstack $ map taskLine $ sort tasks'
      where
        tasks' = case model ^. taskModel . tasks of
          Nothing    -> []
          Just tasks' -> TASK.filterTasks (model ^. calendarModel . dayOfTask) (TASK.makeIndexed tasks')
        taskLine tasks''@(TASK.TaskIndexed (index,task)) = box (box (taskLine' tasks'')
                                 `styleBasic` [padding 10, radius 10,
                                                bgColor $ if model ^. isDoneI index
                                                          then rgbHex taskColor14
                                                          else rgbHex taskColor3])
                           `styleBasic` [padding 5]

        -- | taskline allows to modify priority, done status, text
        taskLine' (TASK.TaskIndexed (index,task)) =
          hstack [ box (hstack [checkbox (isDoneI index), filler]) `styleBasic` [width 50]
                 , spacer
                 , button remixCalendarEventFill (TaskEvent $ UpdateDate index)
                   `styleBasic` Style.styleRemix (if model ^. isDoneI index
                                                  then rgbHex taskColor9
                                                  else rgbHex taskColor16)
                   `styleHover` [textColor $ rgbHex taskColorDelete]
                 , spacer
                 , textDropdownS (isPriorityI index) [TASK.High, TASK.Medium, TASK.Low]
                   `styleBasic` [width 100
                                , radius 0
                                , if model ^. isDoneI index
                                  then border 0 transparent
                                  else border 1 (rgbHex taskColor19)
                                , bgColor transparent]
                 , spacer
                 , textField (isTitleI index) `styleBasic` [radius 0
                                                           , if model ^. isDoneI index
                                                             then border 0 transparent
                                                             else border 1 $ rgbHex taskColor19
                                                           , bgColor transparent]
                 , filler
                 , button remixDeleteBin2Line (TaskEvent $ DeleteTask index)
                   `styleBasic` Style.styleRemix (if model ^. isDoneI index then rgbHex taskColor9 else (rgbHex taskColor16))
                   `styleHover` [textColor $ rgbHex taskColorDelete]
                 ]
