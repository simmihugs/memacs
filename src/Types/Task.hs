{-# LANGUAGE OverloadedStrings #-}
module Types.Task where

import Prelude hiding (readFile,writeFile)
import Data.Semigroup
import qualified Data.Text as T
import Text.XML
import Text.XML.Cursor 
import Control.Exception
import Data.Map (empty)
import qualified Types.Calendar as CALENDAR
import Control.Lens hiding ( element )
import Data.Default


-- | Type to representing the priority of a task.
data Priority = High | Medium | Low deriving Eq
instance Show Priority where
  show High   = "High"
  show Medium = "Medium"
  show Low    = "Low"  

-- | Type to represent a task.  
data Task = Task {
  _title    :: T.Text,
  _text     :: T.Text,
  _date     :: CALENDAR.Days,
  _priority :: Priority,
  _done     :: Bool
  } deriving (Eq,Show)


-- | Default instance is used to create empty task.
instance Default Task where
  def = Task {
    _title    = "titel",
    _text     = "text",
    _date     = 44636,
    _priority = High,
    _done     = False
  }


-- | Needed for sorting of Tasks
instance Ord Task where
  (Task _ _ _ _ True)       `compare` (Task _ _ _ _ True)       = EQ
  (Task _ _ _ _ False)      `compare` (Task _ _ _ _ True)       = LT
  (Task _ _ _ _ True)       `compare` (Task _ _ _ _ False)      = GT
  (Task _ _ _ High False)   `compare` (Task _ _ _ High False)   = EQ
  (Task _ _ _ High False)   `compare` (Task _ _ _ Medium False) = LT
  (Task _ _ _ High False)   `compare` (Task _ _ _ Low False)    = LT
  (Task _ _ _ Medium False) `compare` (Task _ _ _ High False)   = GT
  (Task _ _ _ Medium False) `compare` (Task _ _ _ Medium False) = EQ
  (Task _ _ _ Medium False) `compare` (Task _ _ _ Low False)    = LT
  (Task _ _ _ Low    False) `compare` (Task _ _ _ High False)   = GT
  (Task _ _ _ Low    False) `compare` (Task _ _ _ Medium False) = GT
  (Task _ _ _ Low    False) `compare` (Task _ _ _ Low False)    = EQ


-- | IndexedTasks are used to find task in task list for deletion/change
newtype TaskIndexed = TaskIndexed (Int,Task) deriving (Eq,Show)
instance Ord TaskIndexed where
  (TaskIndexed (_,t1)) `compare` (TaskIndexed (_,t2)) = t1 `compare` t2


-- | Find tasks for spefic day
filterTasks :: CALENDAR.Days -> [TaskIndexed] -> [TaskIndexed]
filterTasks days tasks = do
  filter (\(TaskIndexed (index,Task _ _ date' _ _)) -> date'==days) tasks


-- | Create a IndexedTask 
makeIndexed :: [Task] -> [TaskIndexed]
makeIndexed tasks = zipWith (curry TaskIndexed) [0..length tasks] tasks


-- | Create a Indexed Task list save
makeIndexedMaybe :: Maybe [Task] -> Maybe [TaskIndexed]
makeIndexedMaybe Nothing = Nothing
makeIndexedMaybe (Just tasks) = Just $ makeIndexed tasks

getPriority :: T.Text -> Priority
getPriority "High"   = High
getPriority "Medium" = Medium
getPriority _        = Low


isDone "done" = True
isDone _      = False


writeDone True = T.pack "done"
writeDone _    = T.pack "todo"


-- | Parse task xml into Task list (Unsafe)
readTasks :: FilePath -> IO [Task]
readTasks fileName = do
    doc <- readFile def fileName
    let cursor = fromDocument doc
    let tasks = cursor $// element "task"
          >=> \x ->
                let title'    = T.strip $ T.concat $ content =<< descendant =<< element "title"    =<< child x
                    text'     = T.strip $ T.concat $ content =<< descendant =<< element "text"     =<< child x
                    date'     = tD $ content =<< descendant =<< element "date"                     =<< child x
                    priority' = T.strip $ T.concat $ content =<< descendant =<< element "priority" =<< child x
                    todo'     = T.strip $ T.concat $ content =<< descendant =<< element "done"     =<< child x
                in [Task title' text' date' (getPriority priority') (isDone todo')]
    return tasks
          where
            tD = read . T.unpack . T.concat


-- | Create Text.XML.Node from Task
taskToNode :: Task -> Node
taskToNode (Task title text date priority todo) =
  NodeElement $ Element "task" empty
  [ NodeElement $ Element "title"    empty [ NodeContent title ]
  , NodeElement $ Element "text"     empty [ NodeContent text ]
  , NodeElement $ Element "date"     empty [ NodeContent (T.pack $ show date) ]
  , NodeElement $ Element "priority" empty [ NodeContent (T.pack $ show priority) ]
  , NodeElement $ Element "done"     empty [ NodeContent (writeDone todo) ]]
  

-- | Create Text.XML.Element from Task
tasksToElement :: [Task] -> Element
tasksToElement tasks =
  Element "tasks" empty (map taskToNode tasks)


-- | write Tasks to file (Unsafe)  
writeTasks :: FilePath -> [Task] -> IO ()
writeTasks fileName tasks = do
  let root = tasksToElement tasks 
  writeFile (def { rsPretty = True }) fileName $ Document (Prologue [] Nothing []) root []


-- | write Tasks to file (Safe)
writeTasksMaybe :: FilePath -> Maybe [Task] -> IO Bool
writeTasksMaybe fileName tasks = case tasks of
  Nothing    -> tryWriting fileName []
  Just tasks -> tryWriting fileName tasks
  where
    tryWriting filename tasks = do
      result <- try (writeTasks fileName tasks) :: IO (Either SomeException ())
      case result of
        Left ex -> return False
        Right _ -> return True


makeLenses 'Task

