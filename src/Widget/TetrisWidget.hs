{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

{-
TetrisWidget is a implementation of a Monomerwidget based on
an example from the monomer github

https://github.com/fjvallarino/monomer

specifically on

https://github.com/fjvallarino/monomer/blob/main/examples/generative/Widgets/CirclesGrid.hs

from which it reuses the basic structure an the needed functions
but should be mostly reimplemented - because it works quite
differently from `CirclesGrid.hs`

-}

module Widget.TetrisWidget where

import Control.Lens
import Control.Monad (when)
import Data.Default
import Data.Maybe
import qualified Data.Text as T
import Monomer.Graphics.ColorTable
import Monomer.Widgets.Single
import GHC.Float
import qualified Monomer.Lens as L

-- | My Modules
import Widget.Tetris as Te
import qualified Utils.Style as Style
import Model

-- TetrisState represents the internal state, which cannot
-- be accessed from outside the widget.
data TetrisState = TetrisState {
  _tsField       :: Field,
  _tsPlayer      :: Player,
  _tsPlayerIndex :: Maybe Int,
  _tsPaused      :: Bool,
  _tsEnd         :: Bool,
  _tsPoints      :: Int
} deriving (Eq, Show)

makeLensesWith abbreviatedFields ''TetrisState


-- The constructor of the widget to be used in UI functions.
-- Unlike other models this widget takes a parameter
-- AppModel. This allows when ever there is a change to the
-- AppModel to trigger a change in the tetris widget.
-- this is used to move the tetris bricks down. 
tetris :: AppModel -> WidgetNode s e
tetris model = defaultWidgetNode "tetris" widget where
  widget = makeTetris model (TetrisState field' player' Nothing False False 0)
  -- create a `random` player (different game pieces)
  player' = randomPlayer (model ^. tetrisModel . randomIndex)
  -- use empty field
  field'  = Te.defaultField


-- Calculate for a given Field how many rows are complete.
-- remove the complete rows, inc the points and return a
-- field without empty rows an a number of points.
collectPoints :: Field -> (Int,Field)
collectPoints field = collectPoints' (init $ tail $ field)
  where
    c = [Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall,Wall]
    d = [Wall,Te.Empty,Te.Empty,Te.Empty,Te.Empty,Te.Empty,Te.Empty,Te.Empty,Te.Empty,Te.Empty,Te.Empty,Te.Empty,Te.Empty,Te.Empty,Wall]
    fillUp :: Int -> Field -> Field
    fillUp 0 a = a
    fillUp n a = fillUp (pred n) ([d] ++ a)
    
    collectPoints' list = do
      let oldLength = length list
          newList   = filter (\x -> Te.Empty `elem` x) list
          newLength = length newList
          points    = oldLength - newLength
          newList'  = fillUp points newList
          newList'' = [c]++newList'++[c]
      (100*points,newList'')
 

-- The actual widget creation function
-- See https://github.com/fjvallarino/monomer
-- for more details on how to construct widgets in monomer.
makeTetris :: AppModel -> TetrisState -> Widget s e
makeTetris model state = widget (update state) where
  widget state' = createSingle state' def {
    singleUseScissor = True,
    singleMerge = merge,
    singleHandleEvent = handleEvent,
    singleGetSizeReq = getSizeReq,
    singleRender = render
  }

  -- Update coordinates the progression of the game.
  --
  -- At start of the App a producer
  -- thread is launched which at an interval triggers an
  -- event. This leads to a rebuilding of the ui which
  -- in case of makeTetris reaches update and is used to
  -- move a player down, trigger a new player, or the end
  -- of the game ...
  --
  -- See `Events/TetrisEvents.hs` for more details on the
  -- producer threads.
  --
  update state'
    | not (DisplayTetris == model ^. displayModel) = -- Tetris is not running set everything to start position
        state''
          & player .~ randomPlayer (model ^. tetrisModel . randomIndex)
          & points .~ 0
          & field  .~ Te.defaultField
    | state' ^. end    = state' -- Tetris is running, but the game is over ... dont do anything
    | state' ^. paused = state' -- Tetris is running, but the game is paused ... dont do anything
    | collision BelowSide state'' = state''' -- Key reached bottom
    | otherwise        = do -- Move player down by on square
        state''
          & player .~ (Player (x,y + (model ^. tetrisModel . counter)) tetris' position')
          & playerIndex .~ (if isNothing (state ^. playerIndex)
                             then Just (model ^. tetrisModel . randomIndex)
                             else state'' ^. playerIndex)           
      where
        (Player (x,y) tetris' position') = state' ^. player
        state'' = do 
          let (points',field') = collectPoints (state ^. field)
          state'
            & points +~ points'
            & field .~ field'
        state''' = if y==1 -- collision happend while key spawned ... the end of the game
                   then state'' & end .~ True
                   else do -- spawn a new player
          let newPlayer = randomPlayer (fromJust $ state ^. playerIndex)
              (Player (x,y) tetris' rotation') = state ^. player
              field' = case tetris' of
                         LeftL  -> leftLField    rotation' x y (state' ^. field)
                         RightL -> rightLField   rotation' x y (state' ^. field)
                         Square -> squareField   rotation' x y (state' ^. field)
                         StepL  -> stepLField    rotation' x y (state' ^. field)
                         StepR  -> stepRField    rotation' x y (state' ^. field)
                         Line   -> lineField     rotation' x y (state' ^. field)      
                         _      -> triangleField rotation' x y (state' ^. field)
          state''
            & player .~ newPlayer
            & field .~ field'
            & playerIndex .~ Just (model ^. tetrisModel . randomIndex)

  -- nessecary default functionalyt of widget 
  merge wenv node oldNode oldState = resultNode newNode where
    newNode = node
      & L.widget .~ makeTetris model oldState

  -- used to move tetris key around
  handleEvent wenv node target evt = 
    case evt of
      KeyAction def keycode KeyPressed -> handleKeycode keycode
      _ -> Nothing
    where
      handleKeycode keycode
        | keycode == keyDown  = movePlayerDown
        | keycode == keyRight = movePlayerRight
        | keycode == keyLeft  = movePlayerLeft
        | keycode == keyUp    = movePlayerRotate
        | keycode == keySpace = pauseOrRestart
        | otherwise           = Nothing

      pauseOrRestart = if state ^. end
                       then restart
                       else pauseOrResume
        where
          -- Restart game. Remove all keys from field
          -- set points to 0
          restart = Just (resultReqs newNode [RenderOnce]) where
            state' = state
                     & end .~ False
                     & points .~ 0
                     & paused .~ False
                     & field  .~ Te.defaultField
                     & player .~ randomPlayer (model ^. tetrisModel . randomIndex)
            newNode = node
              & L.widget .~ makeTetris model state'

          -- If currently the game is paused -> resume or vice versa
          pauseOrResume = Just (resultReqs newNode [RenderOnce]) where
            state' = state & paused %~ not
            newNode = node
              & L.widget .~ makeTetris model state'

      -- Move Player Down or to left or right
      movePlayer addX addY side state = Just (resultReqs newNode [RenderOnce]) where
        (Player (x,y) tetris' rotation') = state ^. player
        state' = if collision side state -- If collision is detected, dont move
                 then state
                 else state & player .~
                      (Player (x+addX,y+addY) tetris') rotation'
        newNode = node
          & L.widget .~ makeTetris model state'
          

      -- Rotate Player
      movePlayerRotate = Just (resultReqs newNode [RenderOnce]) where
        state' = if rotationCollision state -- If collision for rotation is detected dont rotate
                 then state
                 else state
                       & player .~ rotatePlayer (state ^. player)
        newNode = node
          & L.widget .~ makeTetris model state'
          

      movePlayerDown  = movePlayer 0    1 BelowSide state 
      movePlayerRight = movePlayer 1    0 RightSide state
      movePlayerLeft  = movePlayer (-1) 0 LeftSide  state

  -- nessecary default function for widget which request space for widget 
  getSizeReq wenv node = (expandSize 100 1, expandSize 100 1)

  -- the actuall drawing
  render wenv node renderer = do
    drawAll model renderer state

-- Detects if a roation for the current player inside
-- the current field would cause a collision.
rotationCollision :: TetrisState -> Bool
rotationCollision state = do
  let (Player (x,y) tetris' rotation') = state ^. player
      field' = state ^. field
      completelyEmpty = take 9 [Te.Empty|a<-[1..]]
      rotationField = completelyEmpty /=
        [ field' ^?! element (y-1) . element x
        , field' ^?! element (y-1) . element (x+1)
        , field' ^?! element (y-1) . element (x-1)                   
        , field' ^?! element y . element x
        , field' ^?! element y . element (x+1)
        , field' ^?! element y . element (x-1)                   
        , field' ^?! element (y+1) . element x
        , field' ^?! element (y+1) . element (x+1)
        , field' ^?! element (y+1) . element (x-1)]                   
      f1 = field' ^?! element y     . element (x+2)
      f2 = field' ^?! element (y+2) . element x    
  case tetris' of
    Line -> if rotation' == Zero
            then rotationField || f1 /= Te.Empty
            else rotationField || f2 /= Te.Empty
    _    -> rotationField
      
-- Detects if for the current player inside
-- the current field with a movement to a given side
-- a collision would occur.
collision :: Side -> TetrisState -> Bool
collision LeftSide state  = collisionLeft state
collision RightSide state = collisionRight state
collision BelowSide state = collisionBelow state


-- Detects if for the current player inside
-- the current field with a movement to the left a collision
-- would occur.
collisionLeft :: TetrisState -> Bool
collisionLeft state = do
  let (Player (x,y) tetris' rotation') = state ^. player
      field' = state ^. field
  case tetris' of
    LeftL  -> collisionLeftLeftL    x y field' rotation'
    RightL -> collisionLeftRightL   x y field' rotation'
    Square -> collisionLeftSquare   x y field' rotation'
    StepL  -> collisionLeftStepL    x y field' rotation'
    StepR  -> collisionLeftStepR    x y field' rotation'
    Line   -> collisionLeftLine     x y field' rotation'
    _      -> collisionLeftTriangle x y field' rotation'


-- Detects if for the current player inside
-- the current field with a movement to the right a
-- collision would occur.
collisionRight :: TetrisState -> Bool
collisionRight state = do
  let (Player (x,y) tetris' rotation') = state ^. player
      field' = state ^. field
  case tetris' of
    LeftL  -> collisionRightLeftL    x y field' rotation'
    RightL -> collisionRightRightL   x y field' rotation'
    Square -> collisionRightSquare   x y field' rotation'
    StepL  -> collisionRightStepL    x y field' rotation'
    StepR  -> collisionRightStepR    x y field' rotation'
    Line   -> collisionRightLine     x y field' rotation'
    _      -> collisionRightTriangle x y field' rotation'


-- Detects if for the current player inside
-- the current field with a movement to the bottom a
-- collision would occur.
collisionBelow :: TetrisState -> Bool
collisionBelow state = do
  let (Player (x,y) tetris' rotation') = state ^. player
      field' = state ^. field
  case tetris' of
    LeftL  -> collisionBelowLeftL    x y field' rotation'
    RightL -> collisionBelowRightL   x y field' rotation'
    Square -> collisionBelowSquare   x y field' rotation'
    StepL  -> collisionBelowStepL    x y field' rotation'
    StepR  -> collisionBelowStepR    x y field' rotation'
    Line   -> collisionBelowLine     x y field' rotation'
    _      -> collisionBelowTriangle x y field' rotation'


-- Helper functions for collision
collisionLeftLeftL :: Int -> Int -> Field -> Rotation -> Bool
collisionLeftLeftL x y field Zero = do
  let f1 = field ^?! element y . element (x-2)
      f2 = field ^?! element (y+1) . element (x-2)
  [f1,f2] /= [Te.Empty,Te.Empty]
collisionLeftLeftL x y field One = do
  let f1 = field ^?! element y . element (x-1)
      f2 = field ^?! element (y-1) . element (x-2)
      f3 = field ^?! element (y+1) . element (x-1)      
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]
collisionLeftLeftL x y field Two = do
  let f1 = field ^?! element y . element (x-2)
      f2 = field ^?! element (y-1) . element x
  [f1,f2] /= [Te.Empty,Te.Empty]
collisionLeftLeftL x y field Three = do
  let f1 = field ^?! element y . element (x-1)
      f2 = field ^?! element (y-1) . element (x-1)
      f3 = field ^?! element (y+1) . element (x-1)
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]

collisionLeftRightL :: Int -> Int -> Field -> Rotation -> Bool
collisionLeftRightL x y field Zero = do
  let f1 = field ^?! element y . element (x-2)
      f2 = field ^?! element (y+1) . element x
  [f1,f2] /= [Te.Empty,Te.Empty]
collisionLeftRightL x y field One = do
  let f1 = field ^?! element y . element (x-1)
      f2 = field ^?! element (y-1) . element (x-1)
      f3 = field ^?! element (y+1) . element (x-1)      
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]
collisionLeftRightL x y field Two = do
  let f1 = field ^?! element y . element (x-2)
      f2 = field ^?! element (y-1) . element (x-2)
  [f1,f2] /= [Te.Empty,Te.Empty]
collisionLeftRightL x y field Three = do
  let f2 = field ^?! element (y-1) . element (x-1)
      f1 = field ^?! element y . element (x-1)
      f3 = field ^?! element (y+1) . element (x-2)      
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]
  

collisionLeftSquare :: Int -> Int -> Field -> Rotation -> Bool
collisionLeftSquare x y field _ = do
  let f1 = field ^?! element y . element (x-2)
      f2 = field ^?! element (y+1) . element (x-2)
  [f1,f2] /= [Te.Empty,Te.Empty]

collisionLeftStepL :: Int -> Int -> Field -> Rotation -> Bool
collisionLeftStepL x y field Zero = do
  let f1 = field ^?! element y . element (x-2)
      f2 = field ^?! element (y+1) . element (x-1)
  [f1,f2] /= [Te.Empty,Te.Empty]
collisionLeftStepL x y field _    = do
  let f3 = field ^?! element (y-1) . element (x-1)      
      f1 = field ^?! element y . element (x-2)
      f2 = field ^?! element (y+1) . element (x-2)
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]

collisionLeftStepR :: Int -> Int -> Field -> Rotation -> Bool
collisionLeftStepR x y field Zero = do
  let f1 = field ^?! element y . element (x-1)
      f2 = field ^?! element (y+1) . element (x-2)
  [f1,f2] /= [Te.Empty,Te.Empty]
collisionLeftStepR x y field _    = do  
  let f3 = field ^?! element (y-1) . element (x-1)      
      f1 = field ^?! element y . element (x-1)
      f2 = field ^?! element (y+1) . element x
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]

collisionLeftLine :: Int -> Int -> Field -> Rotation -> Bool
collisionLeftLine x y field Zero = do
  let f1 = field ^?! element y . element (x-2)
  f1 /= Te.Empty
collisionLeftLine x y field _ = do 
   let f1 = field ^?! element (y-1) . element (x-1)
       f2 = field ^?! element y     . element (x-1)
       f3 = field ^?! element (y+1) . element (x-1)
       f4 = field ^?! element (y+2) . element (x-1)            
   [f1,f2,f3,f4] /= [Te.Empty,Te.Empty,Te.Empty,Te.Empty]

collisionLeftTriangle :: Int -> Int -> Field -> Rotation -> Bool
collisionLeftTriangle x y field Zero  = do
  let f1 = field ^?! element y . element (x-2)
      f2 = field ^?! element (y-1) . element (x-1)
  [f1,f2] /= [Te.Empty,Te.Empty]
collisionLeftTriangle x y field One   = do
  let f3 = field ^?! element (y-1) . element (x-1)      
      f1 = field ^?! element y . element (x-1)
      f2 = field ^?! element (y+1) . element (x-1)
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]
collisionLeftTriangle x y field Two   = do
  let f1 = field ^?! element y . element (x-2)
      f2 = field ^?! element (y+1) . element (x-1)
  [f1,f2] /= [Te.Empty,Te.Empty]
collisionLeftTriangle x y field Three = do
  let f3 = field ^?! element (y-1) . element (x-1)      
      f1 = field ^?! element y . element (x-2)
      f2 = field ^?! element (y+1) . element (x-1)
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]

collisionRightLeftL :: Int -> Int -> Field -> Rotation -> Bool
collisionRightLeftL x y field Zero  = do
  let f1 = field ^?! element y . element (x+2)
  let f2 = field ^?! element (y+1) . element x  
  [f1,f2] /= [Te.Empty,Te.Empty]
collisionRightLeftL x y field One   = do
  let f3 = field ^?! element (y-1) . element (x+1)      
      f1 = field ^?! element y . element (x+1)
      f2 = field ^?! element (y+1) . element (x+1)
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]
collisionRightLeftL x y field Two   = do
  let f1 = field ^?! element y . element (x+2)
      f2 = field ^?! element (y-1) . element (x+2)
  [f1,f2] /= [Te.Empty,Te.Empty]
collisionRightLeftL x y field Three = do
  let f3 = field ^?! element (y-1) . element (x+1)      
      f1 = field ^?! element y . element (x+1)
      f2 = field ^?! element (y+1) . element (x+2)
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]


collisionRightRightL :: Int -> Int -> Field -> Rotation -> Bool
collisionRightRightL x y field Zero  = do
  let f1 = field ^?! element y . element (x+2)
      f2 = field ^?! element (y+1) . element (x+2)
  [f1,f2] /= [Te.Empty,Te.Empty]
collisionRightRightL x y field One  = do
  let f1 = field ^?! element (y-1) . element (x+2)
      f2 = field ^?! element y     . element (x+1)
      f3 = field ^?! element (y+1) . element (x+1)        
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]
collisionRightRightL x y field Two  = do
  let f1 = field ^?! element y . element (x+2)
      f2 = field ^?! element (y+1) . element x
  [f1,f2] /= [Te.Empty,Te.Empty]
collisionRightRightL x y field Three  = do
  let f1 = field ^?! element (y-1) . element (x+1)
      f2 = field ^?! element y     . element (x+1)
      f3 = field ^?! element (y+1) . element (x+1)        
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]

collisionRightSquare :: Int -> Int -> Field -> Rotation -> Bool
collisionRightSquare x y field _ = do
  let f1 = field ^?! element y . element (x+1)
      f2 = field ^?! element (y+1) . element (x+1)
  [f1,f2] /= [Te.Empty,Te.Empty]
  

collisionRightStepL :: Int -> Int -> Field -> Rotation -> Bool
collisionRightStepL x y field Zero = do
  let f1 = field ^?! element y . element (x+1)
      f2 = field ^?! element (y+1) . element (x+2)
  [f1,f2] /= [Te.Empty,Te.Empty]
collisionRightStepL x y field _ = do
  let f1 = field ^?! element (y-1) . element (x+1)
      f2 = field ^?! element y     . element (x+1)
      f3 = field ^?! element (y+1) . element x        
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]

collisionRightStepR :: Int -> Int -> Field -> Rotation -> Bool
collisionRightStepR x y field Zero = do
  let f1 = field ^?! element y . element (x+2)
      f2 = field ^?! element (y+1) . element (x+1)
  [f1,f2] /= [Te.Empty,Te.Empty]
collisionRightStepR x y field _ = do
  let f1 = field ^?! element (y-1) . element (x+1)
      f2 = field ^?! element y     . element (x+2)
      f3 = field ^?! element (y+1) . element (x+2)        
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]

collisionRightLine :: Int -> Int -> Field -> Rotation -> Bool
collisionRightLine x y field Zero = do
  let f1 = field ^?! element y . element (x+3)
  f1 /= Te.Empty
collisionRightLine x y field _    = do  
  let f1 = field ^?! element (y-1) . element (x+1)
      f2 = field ^?! element y     . element (x+1)
      f3 = field ^?! element (y+1) . element (x+1)
      f4 = field ^?! element (y+2) . element (x+1)              
  [f1,f2,f3,f4] /= [Te.Empty,Te.Empty,Te.Empty,Te.Empty]

collisionRightTriangle :: Int -> Int -> Field -> Rotation -> Bool
collisionRightTriangle x y field Zero  = do
  let f1 = field ^?! element y . element (x+2)
      f2 = field ^?! element (y-1) . element (x+1)
  [f1,f2] /= [Te.Empty,Te.Empty]
collisionRightTriangle x y field One   = do
  let f1 = field ^?! element (y-1) . element (x+1)
      f2 = field ^?! element y     . element (x+2)
      f3 = field ^?! element (y+1) . element (x+1)        
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]
collisionRightTriangle x y field Two   = do
  let f1 = field ^?! element y . element (x+2)
      f2 = field ^?! element (y+1) . element (x+1)
  [f1,f2] /= [Te.Empty,Te.Empty]
collisionRightTriangle x y field Three = do  
  let f1 = field ^?! element (y-1) . element (x+1)
      f2 = field ^?! element y     . element (x+1)
      f3 = field ^?! element (y+1) . element (x+1)        
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]
  

collisionBelowSquare :: Int -> Int -> Field -> Rotation -> Bool
collisionBelowSquare x y field _  = do
  let f1 = field ^?! element (y+2) . element x
      f2 = field ^?! element (y+2) . element (x-1)
  [f1,f2] /= [Te.Empty,Te.Empty]

collisionBelowLine :: Int -> Int -> Field -> Rotation -> Bool
collisionBelowLine x y field Zero  = do
  let f1 = field ^?! element (y+1) . element (x-1)
      f2 = field ^?! element (y+1) . element x
      f3 = field ^?! element (y+1) . element (x+1)
      f4 = field ^?! element (y+1) . element (x+2)              
  [f1,f2,f3,f4] /= [Te.Empty,Te.Empty,Te.Empty,Te.Empty]
collisionBelowLine x y field _     = do  
  let f1 = field ^?! element (y+3) . element x
  f1 /= Te.Empty

collisionBelowStepL :: Int -> Int -> Field -> Rotation -> Bool
collisionBelowStepL x y field Zero = do
  let f1 = field ^?! element (y+1) . element (x-1)
      f2 = field ^?! element (y+2) . element x
      f3 = field ^?! element (y+2) . element (x+1)        
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]
collisionBelowStepL x y field _    = do  
  let f1 = field ^?! element (y+1) . element x
      f2 = field ^?! element (y+2) . element (x-1)
  [f1,f2] /= [Te.Empty,Te.Empty]

collisionBelowStepR :: Int -> Int -> Field -> Rotation -> Bool
collisionBelowStepR x y field Zero = do
  let f1 = field ^?! element (y+2) . element (x-1)
      f2 = field ^?! element (y+2) . element x
      f3 = field ^?! element (y+1) . element (x+1)        
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]
collisionBelowStepR x y field _    = do  
  let f1 = field ^?! element (y+1) . element x
      f2 = field ^?! element (y+2) . element (x+1)
  [f1,f2] /= [Te.Empty,Te.Empty]

collisionBelowTriangle :: Int -> Int -> Field -> Rotation -> Bool
collisionBelowTriangle x y field Zero  = do
  let f1 = field ^?! element (y+1) . element (x-1)
      f2 = field ^?! element (y+1) . element x
      f3 = field ^?! element (y+1) . element (x+1)        
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]
collisionBelowTriangle x y field One   = do
  let f1 = field ^?! element (y+2) . element x
      f2 = field ^?! element (y+1) . element (x+1)
  [f1,f2] /= [Te.Empty,Te.Empty]
collisionBelowTriangle x y field Two   = do
  let f1 = field ^?! element (y+1) . element (x-1)
      f2 = field ^?! element (y+2) . element x
      f3 = field ^?! element (y+1) . element (x+1)        
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]
collisionBelowTriangle x y field Three = do  
  let f1 = field ^?! element (y+2) . element x
      f2 = field ^?! element (y+1) . element (x-1)
  [f1,f2] /= [Te.Empty,Te.Empty]

collisionBelowLeftL :: Int -> Int -> Field -> Rotation -> Bool
collisionBelowLeftL x y field Zero  = do
  let f1 = field ^?! element (y+2) . element (x-1)
      f2 = field ^?! element (y+1) . element x
      f3 = field ^?! element (y+1) . element (x+1)        
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]
collisionBelowLeftL x y field One   = do
  let f1 = field ^?! element (y+2) . element x
      f2 = field ^?! element y     . element (x-1)
  [f1,f2] /= [Te.Empty,Te.Empty]
collisionBelowLeftL x y field Two   = do
  let f1 = field ^?! element (y+1) . element (x-1)
      f2 = field ^?! element (y+1) . element x
      f3 = field ^?! element (y+1) . element (x+1)        
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]
collisionBelowLeftL x y field Three = do  
  let f1 = field ^?! element (y+2) . element x
      f2 = field ^?! element (y+2) . element (x+1)
  [f1,f2] /= [Te.Empty,Te.Empty]

collisionBelowRightL :: Int -> Int -> Field -> Rotation -> Bool
collisionBelowRightL x y field Zero  = do
  let f1 = field ^?! element (y+1) . element (x-1)
      f2 = field ^?! element (y+1) . element x
      f3 = field ^?! element (y+2) . element (x+1)        
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]
collisionBelowRightL x y field One   = do
  let f1 = field ^?! element (y+2) . element x
      f2 = field ^?! element y     . element (x+1)
  [f1,f2] /= [Te.Empty,Te.Empty]
collisionBelowRightL x y field Two   = do
  let f1 = field ^?! element (y+1) . element (x-1)
      f2 = field ^?! element (y+1) . element x
      f3 = field ^?! element (y+1) . element (x+1)        
  [f1,f2,f3] /= [Te.Empty,Te.Empty,Te.Empty]
collisionBelowRightL x y field Three = do  
  let f1 = field ^?! element (y+2) . element x
      f2 = field ^?! element (y+2) . element (x-1)
  [f1,f2] /= [Te.Empty,Te.Empty]


-- Draws the actual game field with its borders, all pieces
-- at the ground an the current player.
drawField :: AppModel -> Double -> Double -> Field -> Renderer -> IO ()
drawField model xOffset yOffset field renderer = drawField'' model 0 field renderer
  where
    -- All the colors which are used
    c1 = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "tetris" "tetrisColor1"
    c2 = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "tetris" "tetrisColor2"
    c3 = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "tetris" "tetrisColor3"
    c4 = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "tetris" "tetrisColor4"
    c5 = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "tetris" "tetrisColor5"
    c6 = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "tetris" "tetrisColor6"
    c7 = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "tetris" "tetrisColor7"
    c8 = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "tetris" "tetrisColor7"
    

    -- Helper draw the actual Field, row by row
    drawField'' :: AppModel -> Double -> Field -> Renderer -> IO ()
    drawField'' model n []     renderer = return ()
    drawField'' model n (x:xs) renderer =
      drawRow model n 0 x renderer
      >> drawField'' model (20+n) xs renderer

    -- Helper draw Row, square by square
    drawRow :: AppModel -> Double -> Double -> Row -> Renderer -> IO ()
    drawRow model n m []     renderer = return ()
    drawRow model n m (x:xs) renderer =
      drawItem model n m x renderer
      >> drawRow model n (20+m) xs renderer

    -- Helper draw Square in color based on Tetris type
    drawItem :: AppModel -> Double -> Double -> Tetris -> Renderer -> IO ()
    drawItem model _ _ Te.Empty _        = return ()
    drawItem model n m Wall     renderer = drawSquare' model m n c1 renderer
    drawItem model n m LeftL    renderer = drawSquare' model m n c2 renderer
    drawItem model n m RightL   renderer = drawSquare' model m n c3 renderer
    drawItem model n m Square   renderer = drawSquare' model m n c4 renderer
    drawItem model n m StepL    renderer = drawSquare' model m n c5 renderer
    drawItem model n m StepR    renderer = drawSquare' model m n c6 renderer
    drawItem model n m Line     renderer = drawSquare' model m n c7 renderer
    drawItem model n m Triangle renderer = drawSquare' model m n c8 renderer

    -- The real render function
    drawSquare' :: AppModel -> Double -> Double -> Color -> Renderer -> IO ()
    drawSquare' model x y color renderer = do
      let colorFill = color & L.a .~ 0.3
      beginPath renderer
      setStrokeWidth renderer 2
      setStrokeColor renderer color
      setFillColor renderer colorFill
      renderRect renderer $ Rect (xOffset + 50+x) (yOffset + 50+y) 20 20
      fill renderer
      stroke renderer


-- CreateField with square
squareField :: Rotation -> Int -> Int -> Field -> Field
squareField _ x y field' =
  field'
  & element y     . element x     .~ Square
  & element y     . element (x-1) .~ Square
  & element (y+1) . element x     .~ Square
  & element (y+1) . element (x-1) .~ Square
  

-- CreateField with leftL
leftLField :: Rotation -> Int -> Int -> Field -> Field
leftLField Zero x y field' =
  field'
  & element y     . element (x-1) .~ LeftL
  & element y     . element x     .~ LeftL
  & element y     . element (x+1) .~ LeftL
  & element (y+1) . element (x-1) .~ LeftL                     
leftLField One x y field' =
  field'
  & element (y-1) . element (x-1) .~ LeftL
  & element (y-1) . element x     .~ LeftL
  & element y     . element x     .~ LeftL
  & element (y+1) . element x     .~ LeftL                     
leftLField Two x y field' =
  field'
  & element (y-1) . element (x+1) .~ LeftL                     
  & element y     . element (x-1) .~ LeftL
  & element y     . element x     .~ LeftL
  & element y     . element (x+1) .~ LeftL
leftLField Three x y field' =
  field'
  & element (y-1) . element x     .~ LeftL
  & element y     . element x     .~ LeftL
  & element (y+1) . element x     .~ LeftL                     
  & element (y+1) . element (x+1) .~ LeftL


-- CreateField with rightL
rightLField :: Rotation -> Int -> Int -> Field -> Field
rightLField Zero x y field' =
  field'
  & element y     . element (x-1) .~ RightL
  & element y     . element x     .~ RightL
  & element y     . element (x+1) .~ RightL
  & element (y+1) . element (x+1) .~ RightL
rightLField One x y field' =
  field'
  & element (y-1) . element (x+1) .~ RightL
  & element (y-1) . element x     .~ RightL
  & element y     . element x     .~ RightL
  & element (y+1) . element x     .~ RightL                     
rightLField Two x y field' =
  field'
  & element (y-1) . element (x-1) .~ RightL                    
  & element y     . element (x-1) .~ RightL
  & element y     . element x     .~ RightL
  & element y     . element (x+1) .~ RightL
rightLField Three x y field' =
  field'
  & element (y-1) . element x     .~ RightL
  & element y     . element x     .~ RightL
  & element (y+1) . element x     .~ RightL                     
  & element (y+1) . element (x-1) .~ RightL

-- CreateField with Line
lineField :: Rotation -> Int -> Int -> Field -> Field
lineField Zero x y field' =
  field' 
  & element y . element (x-1) .~ Line
  & element y . element x     .~ Line
  & element y . element (x+1) .~ Line
  & element y . element (x+2) .~ Line
lineField _ x y field' =
  field' 
  & element (y-1) . element x .~ Line
  & element y     . element x .~ Line
  & element (y+1) . element x .~ Line
  & element (y+2) . element x .~ Line

-- CreateField with StepL
stepLField :: Rotation -> Int -> Int -> Field -> Field
stepLField Zero x y field' =
  field'
  & element y     . element x     .~ StepL
  & element y     . element (x-1) .~ StepL
  & element (y+1) . element x     .~ StepL
  & element (y+1) . element (x+1) .~ StepL                     
stepLField _ x y field' =
  field'
  & element (y-1) . element x     .~ StepL
  & element y     . element x     .~ StepL
  & element y     . element (x-1) .~ StepL
  & element (y+1) . element (x-1) .~ StepL                     

-- CreateField with StepR
stepRField :: Rotation -> Int -> Int -> Field -> Field
stepRField Zero x y field' =
  field'
  & element y     . element x     .~ StepR
  & element y     . element (x+1) .~ StepR
  & element (y+1) . element x     .~ StepR
  & element (y+1) . element (x-1) .~ StepR                     
stepRField _ x y field' =
  field'
  & element (y-1) . element x     .~ StepR
  & element y     . element x     .~ StepR
  & element y     . element (x+1) .~ StepR
  & element (y+1) . element (x+1) .~ StepR                     

-- CreateField with Triangle
triangleField :: Rotation -> Int -> Int -> Field -> Field
triangleField Zero x y field' =
  field'
  & element y     . element x     .~ Triangle
  & element y     . element (x-1) .~ Triangle
  & element y     . element (x+1) .~ Triangle
  & element (y-1) . element x     .~ Triangle                     
triangleField One x y field' =
  field'
  & element y     . element x     .~ Triangle
  & element (y-1) . element x     .~ Triangle
  & element (y+1) . element x     .~ Triangle                       
  & element y     . element (x+1) .~ Triangle
triangleField Two x y field' =
  field'
  & element y     . element x     .~ Triangle
  & element y     . element (x-1) .~ Triangle
  & element y     . element (x+1) .~ Triangle
  & element (y+1) . element x     .~ Triangle                     
triangleField Three x y field' =
  field'
  & element y     . element x     .~ Triangle
  & element (y-1) . element x     .~ Triangle
  & element (y+1) . element x     .~ Triangle                       
  & element y     . element (x-1) .~ Triangle


-- Draws the Background when paused 
drawBackground :: AppModel -> Double -> Double -> Renderer -> IO ()
drawBackground model x y renderer = do
  let color  = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "tetris" "tetrisTextColor"
  let color2 = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "tetris" "tetrisTextColor2"
  let colorFill = color & L.a .~ 0.6
  beginPath renderer
  setStrokeWidth renderer 2
  setStrokeColor renderer color
  setFillColor renderer colorFill
  renderRect renderer $ Rect (x + 50) (y + 50) 480 500
  fill renderer
  stroke renderer


-- Draws Pause message
drawPausePanel :: AppModel -> Double -> Double -> Renderer -> TetrisState -> IO ()
drawPausePanel model x y renderer state = do
  drawBackground model x y renderer
  let color = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "tetris" "tetrisTextColor2"
  beginPath renderer
  setStrokeWidth renderer 2
  setStrokeColor renderer color
  setFillColor renderer color
  renderText renderer (Point (x + 200) (y + 300)) (Font "Game") (FontSize 25) (FontSpace 5) "Pause"
  stroke renderer


-- Draws the End message
drawEndPanel :: AppModel -> Double -> Double -> Renderer -> TetrisState -> IO ()
drawEndPanel model x y renderer state = do
  drawBackground model x y renderer
  let color = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "tetris" "tetrisTextColor2"
  let message1 = T.pack $ "You achieved: "
  let message2 = T.pack $  (show $ state ^. points) ++ " points."
  let message3 = T.pack $ "Press SPACE to "
  let message4 = T.pack $ "play again."
  beginPath renderer
  setStrokeWidth renderer 2
  setStrokeColor renderer color
  setFillColor renderer color
  renderText renderer (Point (x + 200) (y + 200)) (Font "Game") (FontSize 25) (FontSpace 5) "Done!!!"
  renderText renderer (Point (x + 100) (y + 250)) (Font "Game") (FontSize 25) (FontSpace 5) message1
  renderText renderer (Point (x + 100) (y + 300)) (Font "Game") (FontSize 25) (FontSpace 5) message2
  renderText renderer (Point (x + 100) (y + 350)) (Font "Game") (FontSize 25) (FontSpace 5) message3
  renderText renderer (Point (x + 100) (y + 400)) (Font "Game") (FontSize 25) (FontSpace 5) message4
  stroke renderer


drawAll :: AppModel -> Renderer -> TetrisState -> IO ()
drawAll model renderer state = do
  let color = rgbHex $ Style.lookupUnsafe (model ^. settingsModel . settings) "tetris" "tetrisTextColor"
      colorFill = color & L.a .~ 0.3
      text' = T.pack $ "Points:"
      text'' = T.pack $ (show $ state ^. points)
      x' = 200
      y' = 0

  -- Draw the current Points
  beginPath renderer
  setStrokeWidth renderer 2
  setStrokeColor renderer color
  setFillColor renderer color
  renderText renderer (Point (x' + 370) (y' + 200)) (Font "Game") (FontSize 18) (FontSpace 5) text'
  renderText renderer (Point (x' + 370) (y' + 230)) (Font "Game") (FontSize 18) (FontSpace 5) text''
  stroke renderer

  -- Draw the game field
  drawField model (320 + x') (0 + y') field' renderer

  -- Draw PausePanel if game is paused 
  if state ^. paused then drawPausePanel model x' y' renderer state else return ()

  -- Draw EndPanel if game is ended
  if state ^. end then drawEndPanel model x' y' renderer state else return ()

  -- Draw preview of upcoming field 
  case tetris' of
    LeftL    -> drawLeftL'    x' y'
    RightL   -> drawRightL'   x' y'
    Square   -> drawSquare'   x' y'
    StepL    -> drawStepL'    x' y'
    StepR    -> drawStepR'    x' y'
    Line     -> drawLine'     x' y'
    Triangle -> drawTriangle' x' y'
    _        -> return ()
    where
      field' = do
        let tetris'' = if isNothing (state ^. playerIndex)
                       then Te.Empty
                       else tetrisKeys!!(fromJust $ state ^. playerIndex)
        case tetris'' of
          LeftL    -> leftLField    Zero 3 3 previewField
          RightL   -> rightLField   Zero 3 3 previewField
          Square   -> squareField   Zero 3 3 previewField
          StepL    -> stepLField    Zero 3 3 previewField
          StepR    -> stepRField    Zero 3 3 previewField
          Line     -> lineField     Zero 3 3 previewField
          Triangle -> triangleField Zero 3 3 previewField
          _        -> previewField          
      (Player (x,y) tetris' rotation') = state ^. player
      drawSquare'   x' y' = drawField model x' y' (squareField   rotation' x y (state ^. field)) renderer
      drawRightL'   x' y' = drawField model x' y' (rightLField   rotation' x y (state ^. field)) renderer
      drawLeftL'    x' y' = drawField model x' y' (leftLField    rotation' x y (state ^. field)) renderer
      drawStepL'    x' y' = drawField model x' y' (stepLField    rotation' x y (state ^. field)) renderer
      drawStepR'    x' y' = drawField model x' y' (stepRField    rotation' x y (state ^. field)) renderer
      drawLine'     x' y' = drawField model x' y' (lineField     rotation' x y (state ^. field)) renderer
      drawTriangle' x' y' = drawField model x' y' (triangleField rotation' x y (state ^. field)) renderer 

