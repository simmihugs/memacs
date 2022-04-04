{-# LANGUAGE OverloadedStrings #-}
module Utils.FileBrowser where

import Prelude hiding (readFile,writeFile)
import Data.Semigroup ()
import qualified Data.Text as T
import Text.XML ()
import Text.XML.Cursor () 
import Control.Exception ( try, SomeException )
import Data.Map (empty)
import Control.Lens ()
import Data.Default ()
import System.Directory
    ( getCurrentDirectory, getHomeDirectory, listDirectory )
import qualified System.FilePath as F



-- | My Modules
import Model ( FileType(..) )
import qualified Types.Calendar as CALENDAR
import qualified Utils.Tools as Tools


-- Safe get Homedir function
-- Writes to log.
getHomeDir_ :: FilePath -> IO (Maybe FilePath)
getHomeDir_ errorLogFilePath = do
  result <- try getHomeDirectory :: IO (Either SomeException FilePath)
  case result of
    Left ex -> Tools.writeLog errorLogFilePath "ERROR: Could not determine the home directory."
               >> return Nothing
    Right filePath -> Tools.writeLog errorLogFilePath ("SUCCESS: The home directory is: " ++ filePath)
                      >> return (Just filePath)


-- UnSafe get Homedir function
-- Returns Hardcoded value on failure
getHomeDir :: FilePath -> IO FilePath
getHomeDir errorLogFilePath = do
  homeDir <- getHomeDir_ errorLogFilePath
  case homeDir of
    Nothing       -> return "C://Users//Simon"
    Just filePath -> return filePath


-- Determines wether a FilePath is a file or a directory
isDir :: FilePath -> FileType
isDir x = if F.hasExtension x
          then TypeFile
          else TypeDirectory


-- | Determine wether or not a directory is locked
isLocked :: FileType -> FilePath -> IO Bool
isLocked TypeFile _        = return False
isLocked fileType filePath = do
  result <- try (listDirectory filePath) :: IO (Either SomeException [FilePath])
  case result of
    Left  _ -> return True
    Right _ -> return False


-- | Determine wether or not a list of directories are locked
areLocked :: [(FileType, FilePath)] -> IO [Bool]
areLocked input = do mapM (uncurry isLocked) input


-- | Return all Files in the current Directory
currentFiles :: IO [FilePath]
currentFiles = getCurrentDirectory >>= listDirectory

listCurrentFiles :: IO [(FileType, FilePath)]
listCurrentFiles = do map (\x -> (isDir x,x)) <$> currentFiles


-- | Lists all Files in a given directory
-- | with the designated Filetype and wether
-- | or not a directory is locked
listFiles :: FilePath -> IO [(Bool,FileType, FilePath)]
listFiles filePath = do
  files <- listDirectory filePath
  let fileTypes = map (isDir . (filePath F.</>)) files
  locked <- areLocked $ zip fileTypes $ map (filePath F.</>) files
  return $ zip3 locked fileTypes files


