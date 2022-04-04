{-# LANGUAGE OverloadedStrings #-}
module Events.TetrisEvents where

import Control.Concurrent (threadDelay)
import Control.Lens
import Data.Text ( Text, pack ) 
import Data.Time
import Data.Default
import Monomer
import System.Random ( mkStdGen, randomIO, setStdGen )
import qualified Data.Text as T
import qualified Monomer.Lens as L
import Control.Monad.Random
import GHC.Float

-- | For TChan stuff
import Control.Applicative
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM.TChan
import GHC.Conc
import System.Random
import Data.Time

-- | My Modules
import Model

-- | producer runs continously on separate thread
-- | send msg to main app at fixed intervals 
timeOfDayProducer sendMsg = do
  time <- getLocalTimeOfDay
  sendMsg (TetrisEvent $ TetrisSetTime time)
  threadDelay $ 1000 * 1000
  timeOfDayProducer sendMsg


-- | Task runs, writes into tchan gamestate
-- | finishes
startTed gameState' = do
  atomically $ unGetTChan gameState' Running
  return EmptyEvent


-- | producer runs continously on separate thread
-- | peeks at gamestate (does not empty the queue) 
ted queue' gameState' sendMsg = do
  gameState'' <- atomically $ peekTChan gameState'
  case gameState'' of
    -- | If not running wait then reiterate
    NotRunning -> do
      threadDelay $ 1000 * 1000 * 50
      ted queue' gameState' sendMsg
    -- | If running, peek at queue and maybe reduce value
    -- | queue value determines wait in tetristicker
    -- | producer
    Running -> do
      currentVal'' <- atomically $ peekTChan queue'
      if currentVal'' > 100
        then do
        atomically $ unGetTChan queue' (currentVal'' - 200)
        threadDelay $ 1000 * 1000 * 50
        ted queue' gameState' sendMsg
        else do ted queue' gameState' sendMsg
    -- | If Restarting, reset queue value to default
    Restart -> do
      currentVal'' <- atomically $ peekTChan queue'
      if currentVal'' /=900
        then do
        atomically $ unGetTChan queue' 900
        ted queue' gameState' sendMsg
        else do ted queue' gameState' sendMsg


-- | Task runs, writes into tchan gamestate
-- | finishes
unted gameState' = do
  atomically $ unGetTChan gameState' Restart
  return EmptyEvent


-- | Producer ticker the game
-- | Needed for game piece to move down
-- | sends a Message to app
-- | waits for time independend on queue
runGame sendMsg = do
  index' :: Int <- randomIO
  let index'' = index' `mod` 7
  time <- getLocalTimeOfDay
  sendMsg (TetrisEvent $ TetrisSetTicker time index'')
  threadDelay $ 1000 * 1000 
  runGame sendMsg


-- | Producer ticker the game
-- | Needed for game piece to move down
-- | sends a Message to app
-- | waits for time dependend on queue
runGame3 queue sendMsg = do
  index' :: Int <- randomIO
  let index'' = index' `mod` 7
  time <- getLocalTimeOfDay
  sendMsg (TetrisEvent $ TetrisSetTicker time index'')
  d <- atomically $ peekTChan queue
  threadDelay $ 1000 * d
  runGame3 queue sendMsg


-- | Get time of day
getLocalTimeOfDay :: IO TimeOfDay
getLocalTimeOfDay = do
  time <- getZonedTime
  return . localTimeOfDay . zonedTimeToLocalTime $ time


handleTetrisEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> TetrisAction
  -> [AppEventResponse AppModel AppEvent]
handleTetrisEvent wenv node model evt = case evt of
  -- end tetris game && dont set display to none (opening new thing is possible)
  TetrisStop     -> [Model $ model
                       & tetrisModel . timeTicker .~ 0
                     , Task $ unted (model ^. channelModel . gameState)
                     , Task $ return $ AppendErrorMessage
                       $ pack ("TETRIS_ACTION: Stopped Tetris.\n")
                     ]

  -- start producers, dont show tetris game yet
  TetrisInit     -> [Producer $ timeOfDayProducer 
                    , Producer $ runGame3 (model ^. channelModel . queue)
                    , Task $ startTed (model ^. channelModel . gameState)
                    , Task $ return $ AppendErrorMessage
                      $ pack ("TETRIS_ACTION: Initialized TChannels for Tetris.\n")]

  -- start tetris game
  TetrisStart    -> [Model $ model & displayModel .~ DisplayTetris
                    , Producer $ ted (model ^. channelModel . queue) (model ^. channelModel . gameState)
                    , SetFocusOnKey "tetris"
                    , Task $ return $ AppendErrorMessage
                      $ pack ("TETRIS_ACTION: Started Tetris.\n")]

  -- end tetris game && dont display something else (end tetris without opening new thing)
  TetrisEnd       -> [Model $ model
                      & displayModel .~ DisplayNone
                      & tetrisModel . timeTicker .~ 0
                     , SetFocusOnKey "startButton"
                     , Task $ unted (model ^. channelModel . gameState)
                     , Task $ return $ AppendErrorMessage
                       $ pack ("TETRIS_ACTION: Ended Tetris.\n")]

  -- provide input for tetris ui to update (not widget but timelabel)
  TetrisSetTime time -> fadeInMsg time ++ [Model $ model & tetrisModel . timeTicker +~ 1
                                          , Task $ return $ AppendErrorMessage
                                            $ pack ("TETRIS_ACTION: Set timeticker.\n")]
    where
      fadeInMsg time
        | truncate (todSec time) `mod` 10 /= 0 = []
        | otherwise = [Message "fadeTimeLabel" AnimationStart]

  -- provide AppEvent to Tetris to tricker redraw (key moves down one step)
  TetrisSetTicker _ i1 -> [Model $ model & tetrisModel . counter .~ 1
                      , Model $ model & tetrisModel . randomIndex .~ i1]

