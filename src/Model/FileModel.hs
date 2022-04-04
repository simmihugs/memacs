module Model.FileModel where


import Control.Lens
import Data.Text (Text)
import Data.Time
import Monomer


type FileInfo = (Bool, FilePath, Text)
  
data FileModel = FileModel {
  _fmFiles                :: Maybe [FileInfo], -- openfiles
  _fmShowFiles            :: Bool,
  _fmIndexCurrentFile     :: Int, -- index in _fmFiles
  _fmShowLineNumbers      :: Bool,
  _fmHighlightCurrentLine :: Bool,
  _fmFShowSyntax          :: Bool
  } deriving (Eq, Show)

makeLensesWith abbreviatedFields ''FileModel

data WriteAction = StartWriting
                 | StopWriting
                 deriving (Eq, Show)

data FileAction = OpenFile FileInfo
                | UpdateName Int FilePath
                | SetChanged Int
                | SetUnChanged Int String
                | SaveFile Int
                | ShowFile Int
                | CloseWithoutSaving Int
                deriving (Eq, Show)
