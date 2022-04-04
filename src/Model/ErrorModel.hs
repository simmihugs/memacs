module Model.ErrorModel where 

import Control.Lens
import Data.Text (Text)
import Data.Time
import Monomer


data ErrorModel = ErrorModel {
  _emErrorLogFilePath :: FilePath,
  _emErrorLog         :: Text
  } deriving (Eq, Show)


makeLensesWith abbreviatedFields ''ErrorModel

