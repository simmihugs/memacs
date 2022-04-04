module Model.SettingsModel where 

import Control.Lens
import Data.Text (Text)
import Data.Time
import Monomer
import Parser.Json

data SettingsModel = SettingsModel
  {
    _smSettings :: JValue,
    _smFilePath :: FilePath,
    _smFonts    :: [Text],
    _smUnsaved  :: Bool
  }  deriving (Eq, Show)


makeLensesWith abbreviatedFields ''SettingsModel

data SettingsAction = ShowIt
                    | SaveSettings
                    | StoreColor String String String 
                    | UpdateFont String String String 
                    | ColorPicker String String
                    deriving (Eq, Show)

