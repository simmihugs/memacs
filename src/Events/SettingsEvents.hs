{-# LANGUAGE OverloadedStrings #-}
module Events.SettingsEvents where

import Control.Lens
import Monomer
import Data.List
import Data.Text ( pack, Text )
import Graphics.UI.TinyFileDialogs

-- | My modules
import qualified Types.Calendar as CALENDAR
import qualified Types.Task as TASK
import qualified Utils.Tools as Tools
import qualified Utils.Style as Style
import Events.TetrisEvents
import Parser.Hex
import Model


handleSettingsEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> SettingsAction
  -> [AppEventResponse AppModel AppEvent]
handleSettingsEvent wenv node model x = case x of
  -- Show or Hide Settings
  ShowIt                    -> [Task $ return $ TetrisEvent TetrisStop
                               , Model $ model
                                 & displayModel %~ showOrHide DisplaySettings
                               , Task $ return $ AppendErrorMessage
                                 $ pack ("SETTINGS_ACTION: Show/hide settings pressed\n")]

  -- Save Settings to file
  SaveSettings              -> [Task $ saveSettings
                               , Model $ model & settingsModel . unsaved .~ False]
    where
      saveSettings = do
        let filePath' = model ^. settingsModel . filePath
        let content'  = pack $ show $ model ^. settingsModel . settings
        result <- Tools.tryWriteFile filePath' content'
        return $ AppendErrorMessage (pack $ snd result)

  -- Store Color into settings
  StoreColor cstr key1 key2 -> [Model $ model
                                 & settingsModel . settings %~ Style.storeUnsafe cstr key1 key2
                                 & settingsModel . unsaved .~ True]

  -- Update font of text Area (need to modify file so it redisplays)
  UpdateFont cstr key1 key2 -> [Model $ model 
                                 & settingsModel . settings %~ Style.storeUnsafe cstr key1 key2
                                 & fileModel . files %~ updateText
                                 & settingsModel . unsaved .~ True]
    where
      updateText Nothing      = Nothing
      updateText (Just files) = Just $ files & element (model ^. fileModel . indexCurrentFile) . _3 %~ appendChar
      appendChar text = text <> " "
        
  -- Open color picker
  ColorPicker key1 key2     -> [Task $ colorPicker key1 key2]
    where
      colorPicker key1 key2 = do
        let color = toRGB $ Style.lookupUnsafe (model ^. settingsModel . settings) key1 key2
        result <- colorChooser "ColorChooser" color
        case result of
          Nothing     -> return $ EmptyEvent
          Just color' -> do
            let colorString = fromRGB color'
            return $ SettingsEvent (StoreColor colorString key1 key2)
  
