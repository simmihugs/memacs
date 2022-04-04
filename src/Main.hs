{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Lens
import Data.Text (Text) 
import Data.Time
import Data.Default
import Monomer
import System.Random ( mkStdGen, randomIO, setStdGen )
import qualified Data.Text as T
import qualified Monomer.Lens as L
import Control.Monad.Random
import GHC.Float
import Data.Maybe

-- | For TChan stuff
-- | Used for Communication between main app and
-- | producer threads.
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.Chan
import Control.Concurrent.STM.TChan
import GHC.Conc
import System.Random
import Data.Time


import qualified Utils.Tools       as Tools
import qualified Utils.FileBrowser as FileBrowser
import qualified Utils.Style       as Style


-- | Monomer works
-- | - with a model
-- | - a UI function
-- | - an eventhandler function
-- | The central modules import and forwar their submodules.
import Model
import qualified MainUI     as MainUI
import qualified MainEvents as MainEvents


-- | Loading and handling of default settings.
import qualified Data.HashMap.Strict as DH
import Parser.QQ
import Parser.Json
import qualified Parser.Parser as Parser


-- | Read settings from JSON file.
newSettings :: FilePath -> IO (Maybe (JValue, String))
newSettings filePath = do
  result <- Tools.tryOpenFile filePath
  case result of
    (False,_,_) -> return Nothing
    (_,_,text)  -> return $ Parser.run json (T.unpack text)


-- | Update default settings by reading in JSON file with
-- | different settings.
-- |
-- | Loaded Settings <= default settings 
-- | Settings file does not need to contain all default
-- | parameters.
transFormSettings :: FilePath -> IO JValue
transFormSettings filePath = do
  newSettings' <- newSettings filePath
  case newSettings' of
    Nothing    -> return Style.defaultSettings
    Just (hash1,_) -> case hash1 of
      JObject hash2 -> do
        let defaultSettings' = (\(JObject h) -> h) Style.defaultSettings
        return $ JObject $ DH.mapWithKey (\k v -> JObject $ DH.mapWithKey (\ k2 v2 -> maybeLookUpValue hash2 k k2 v2) ((\(JObject x) -> x) v)) defaultSettings'
      _             -> return Style.defaultSettings


-- | Try to lookup Value in given settings cateory with key1
-- | at postion key2. If value exist return value, else
-- | return given alternative value.
maybeLookUpValue :: DH.HashMap String JValue -> String -> String -> JValue -> JValue
maybeLookUpValue newSettings key1 key2 value2 = do
  let maybeCategory = newSettings DH.!? key1
  case maybeCategory of
    Nothing -> value2
    Just ca -> case ca of
      JObject ca -> do
        let mabyeValue = ca DH.!? key2
        case mabyeValue of
          Nothing -> value2
          Just va -> va
      _          -> value2


main :: IO ()
main = do
  -- | Start Channels for game and seed random generator
  -- | Randomvalues are used for `random` tetris keys.
  -- | queue and game state for commuication between
  -- | producer threads and main app.
  setStdGen $ mkStdGen 42
  queue' <- atomically newTChan
  gameState' <- atomically newTChan  
  atomically $ writeTChan queue' 800
  atomically $ writeTChan gameState' NotRunning

  -- | FilePath for ErrorLog
  -- | ErrorLog allows to log in response of events.
  let errorLogFilePath' = "./assets/config/error.log"
  -- | Create Model to keep track of error data.
  let errorModel'    = ErrorModel errorLogFilePath' ""

  -- | FilePath for Taskfile
  -- | Tasks can be viewed, edited and delted in calendar
  -- | area.
  let taskFilePath' = "./assets/config/tasks.xml"
  tasks' <- Tools.loadTasks taskFilePath' errorLogFilePath'
  -- | Create Model to keep track of task data. 
  let taskModel'     = TaskModel tasks' False taskFilePath' False 0

  -- | Get current date, write to errorlog if failure.
  today' <- Tools.getDate errorLogFilePath'
  -- | Create Model to keep track of calendar data.
  let calendarModel' = CalendarModel today' today' today'

  -- | Get homeDir
  homeDir' <- FileBrowser.getHomeDir errorLogFilePath'
  -- | Get file list from homeDir
  files'   <- FileBrowser.listFiles homeDir'
  -- | Create Model to keep track of file data
  -- | separated into filebrowser and file model.
  let fileBrowserModel' = FileBrowserModel {
        _fbShowFileBrowser  = True,
        _fbHomeDirectory    = homeDir',
        _fbCurrentDirectory = homeDir',
        _fbDirContent       = files',
        _fbShowHidden       = False,
        _fbShowLocked       = False }
  let fileModel'     = FileModel Nothing False 0 True True True 

  -- | Settings from settings json are used to update values
  -- | in default settings.
  let settingsJson   = "./assets/config/config.json"
  settings' <- transFormSettings settingsJson
  -- | Create Model to keep track of settings data.
  let settingsModel' = SettingsModel settings' settingsJson ["Regular","Bold","Game", "Mono1", "Mono2", "Mono3", "Mono4", "Mono5", "Mono6", "Mono7"] False

  -- | Create Model to keep track of channel data.
  let channelModel' = ChannelModel queue' gameState'

  -- | Create Model to keep trak of commandline data.
  let commandLineModel' = CommandLineModel "" False "" False
  
  -- | Create Model to keep track of data needed by 
  -- | tetris Widget.
  let tetrisModel = TetrisModel 0 0 3

  -- | Create Model for the entire app. This model allows to
  -- | access and modify (inside eventhandling) data of all
  -- | models created above.
  let model = AppModel {
        _amErrorModel       = errorModel',
        _amSettingsModel    = settingsModel',
        _amCalendarModel    = calendarModel',
        _amTaskModel        = taskModel',
        _amTetrisModel      = tetrisModel,
        _amDisplayModel     = DisplayNone,
        _amFileModel        = fileModel',
        _amFileBrowserModel = fileBrowserModel',
        _amChannelModel     = channelModel',
        _amCommandLineModel = commandLineModel'
        }

  -- Start app.
  -- `startApp` requires
  --
  -- model - represents data/state of app
  -- changes to model trigger a rerender.
  --
  -- MainEvents.handleEvent - Allows to change model, 
  -- perform IO ...
  --
  -- MainUI.mainUI - used to construct the gui. Can read
  -- model data, but not change it. Can create Events in
  -- response to ui events which will be handled by
  -- handleEvent --> modify model ==> rerender app
  startApp model MainEvents.handleEvent MainUI.mainUI config
  where
    -- | config nessecary for prerequisits.
    config = [
      appWindowTitle "Memac",
      appTheme darkTheme,
      appWindowIcon "./assets/images/icon.bmp",

      -- | Initial Event at startup.
      appInitEvent (TetrisEvent TetrisInit),

      -- | Inside App Fonts can be refernced by key.
      appFontDef "Regular" "./assets/fonts/Roboto-Regular.ttf",
      appFontDef "Bold" "./assets/fonts/Roboto-Bold.ttf",
      appFontDef "Game" "./assets/fonts/press-start/prstart.ttf",
      appFontDef "Remix" "./assets/fonts/remixicon.ttf",
      appFontDef "Mono1" "./assets/fonts/cascadia-code/Cascadia.ttf",
      appFontDef "Mono2" "./assets/fonts/droid-sans-mono/DroidSansMono.ttf",
      appFontDef "Mono3" "./assets/fonts/cousine/Cousine-Regular.ttf",
      appFontDef "Mono4" "./assets/fonts/Bitstream-Vera-Sans-Mono/VeraMono.ttf",
      appFontDef "Mono5" "./assets/fonts/Anonymous-Pro/Anonymous_Pro.ttf",
      appFontDef "Mono6" "./assets/fonts/camingocode/CamingoCode-Regular.ttf",
      appFontDef "Mono7" "./assets/fonts/BPmono/BPmono.ttf"            
      ]


 
