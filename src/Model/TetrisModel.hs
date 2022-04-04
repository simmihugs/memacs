module Model.TetrisModel where 

import Control.Lens
import Data.Text (Text)
import Data.Time
import Monomer



data TetrisModel = TetrisModel {
  _tmTimeTicker  :: Int, -- only needed for ui
  _tmCounter     :: Int, -- always 1 needed to move piece down
  _tmRandomIndex :: Int  -- index for next tetris piece
} deriving (Eq, Show)


makeLensesWith abbreviatedFields ''TetrisModel

data TetrisAction = TetrisInit
                  | TetrisStart
                  | TetrisStop  
                  | TetrisEnd   
                  | TetrisSetTime TimeOfDay
                  | TetrisSetTicker TimeOfDay Int
                  deriving (Eq, Show)
