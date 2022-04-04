module Model.FileBrowserModel where 

import Control.Lens
import Data.Text (Text)
import Data.Time
import Monomer


type DirInfo = (Bool, FileType, FilePath)
data FileType = TypeFile | TypeDirectory deriving (Show, Eq)


data FileBrowserModel = FileBrowserModel {
  _fbShowFileBrowser  :: Bool, -- can be hidden
  _fbHomeDirectory    :: FilePath,
  _fbCurrentDirectory :: FilePath,
  _fbDirContent       :: [DirInfo], -- needs to be updated when switching between directories
  _fbShowHidden       :: Bool,
  _fbShowLocked       :: Bool
  } deriving (Eq, Show)


makeLensesWith abbreviatedFields ''FileBrowserModel


data FileBrowserAction = Updir
                       | Storedir FilePath [DirInfo] 
                       | OpenFileOrDir (FileType, FilePath)
                       | GoHome
                       | OpenDirectoryDialog
                       | ShowHidden
                       | ShowLocked
                       | ShowFileFileBrowser
                       | OpenTextArea
                       deriving (Eq, Show)
