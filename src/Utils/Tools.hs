{-# LANGUAGE OverloadedStrings #-}

{-
Helper functions to read an write files and access the
current date.
-}

module Utils.Tools where

import Prelude hiding (readFile, writeFile)
import Control.Exception
import Data.Text (Text, pack)
import Data.Text.IO (readFile, writeFile)

-- | My modules
import Model
import Types.Calendar as CALENDAR
import Types.Task as TASK

-- Try to append to given file. On failure ignore exception.
writeLog :: FilePath -> String -> IO ()
writeLog filePath errorMessage = do
  _ <- try (appendFile filePath errorMessage ) :: IO (Either SomeException ())
  return ()

-- Try reading file and parsing it's content with xml-conduit
-- into list. Return maybe the result or nothing.
-- loadTasks writes to log.
loadTasks :: FilePath -> FilePath -> IO (Maybe [TASK.Task])
loadTasks fileName errorLogFilePath = do
  tasks' <- try (readTasks fileName) :: IO (Either SomeException [TASK.Task])
  case tasks' of
    Left  ex      -> writeLog errorLogFilePath ("ERROR: Caught exception: " ++ show ex ++ "\n")
                     >> return Nothing 
    Right tasks'' -> writeLog errorLogFilePath ("SUCCESS: Tasks read from file: " ++ fileName ++ "\n")
                     >> return (Just tasks'')


-- Try to get the current date.
-- If Failure, return random day.
-- getDate writes to Log
getDate :: FilePath -> IO Days
getDate errorLogFilePath = do
  today' <- try (CALENDAR.dateToDays_ <$> CALENDAR.today) :: IO (Either SomeException Days)
  case today' of
    Left ex       -> writeLog errorLogFilePath ("ERROR: Caught exception: " ++ show ex ++ "\n")
                     >> return 44600
    Right today'' -> writeLog errorLogFilePath ("SUCCESS: Date of today read: " ++ show today'' ++ "\n")
                     >> return today''


-- Try open a file.
-- If Failure, return FileInfo - Invalid File
tryOpenFile :: FilePath -> IO FileInfo
tryOpenFile filePath = do
  result <- try (readFile filePath) :: IO (Either SomeException Text)
  case result of
    Left ex     -> return (False, filePath, "Invalid File.")
    Right text' -> return (True, filePath, text')


-- Try writing a text into a file
-- Return succesmessage
tryWriteFile :: FilePath -> Text -> IO (Bool,String)
tryWriteFile filePath fileContent = do
  result <- try (writeFile filePath fileContent) :: IO (Either SomeException ())
  case result of
    Left  ex    -> return (False, "ERROR: Write returned exception" ++ show ex)
    Right text' -> return (True, "SUCCESS: Wrote fileContent to file: " ++ filePath)

