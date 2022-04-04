module Model.CommandLineModel where


import Control.Lens
import Data.Text (Text)
import Data.Time
import Monomer

  
data CommandLineModel = CommandLineModel {
  _clmCommandLine        :: Text,
  _clmCommandLineVisible :: Bool,
  _clmErrorLine          :: Text, 
  _clmShowErrorLine      :: Bool
  } deriving (Eq, Show)

makeLensesWith abbreviatedFields ''CommandLineModel

data CommandLineAction = ShowCommandLine
                       | ExecuteCommandLine
                       | Executed
                       | NewFile
                       | ShowSettings
                       | ShowError Text
                       | AbortError
                       | TryOpenFile FilePath
                       | SaveAs FilePath
                       | TryOpenFileDialog
                       | CommandOpenFile (Bool, FilePath, Text)
                       deriving (Eq, Show)
