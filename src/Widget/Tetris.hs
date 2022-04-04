{-
In Widget.Tetris data representations for the tetris game and
helper functions are defined. See Widget.TetrisWidget on how
they are used.
-}

module Widget.Tetris where

import Control.Lens ( makeLenses )

-- Datatype to represent all available types of squares.
-- These types dont represent the actual game piece but
-- rather what kind of brick it is -> is used to handle
-- color and colition and rotation capablities.
data Tetris = Wall
            | LeftL
            | RightL
            | Square
            | StepL
            | StepR
            | Line
            | Triangle
            | Empty
            deriving (Show,Eq)

type Field = [[Tetris]]
type Row   = [Tetris]

data Rotation = Zero | One | Two | Three deriving (Eq, Show)

data Side = LeftSide | BelowSide | RightSide deriving (Eq, Show)

-- The actual tetris game pieces are represented by players.
data Player = Player {
  _position :: (Int,Int),
  _tetris :: Tetris,
  _rotation :: Rotation
  } deriving (Show, Eq)

makeLenses ''Player

-- All available tetris game pieces - without wall an none
tetrisKeys :: [Tetris]
tetrisKeys = [LeftL, RightL, Square, StepL, StepR, Line, Triangle]

-- Originaly intended to be random. Given a random number as
-- index `randomly` creates a Player
randomPlayer index' = Player (2+5,1) (tetrisKeys!!index') Zero

-- Rotate game piece
-- based on game piece an current rotation.
rotatePlayer :: Player -> Player
rotatePlayer square@(Player _ Square _) = square
rotatePlayer (Player p Triangle Zero)   = (Player p Triangle One)
rotatePlayer (Player p Triangle One)    = (Player p Triangle Two)
rotatePlayer (Player p Triangle Two)    = (Player p Triangle Three)
rotatePlayer (Player p Triangle Three)  = (Player p Triangle Zero)
rotatePlayer (Player p RightL Zero)     = (Player p RightL One)
rotatePlayer (Player p RightL One)      = (Player p RightL Two)
rotatePlayer (Player p RightL Two)      = (Player p RightL Three)
rotatePlayer (Player p RightL Three)    = (Player p RightL Zero)
rotatePlayer (Player p LeftL Zero)      = (Player p LeftL One)
rotatePlayer (Player p LeftL One)       = (Player p LeftL Two)
rotatePlayer (Player p LeftL Two)       = (Player p LeftL Three)
rotatePlayer (Player p LeftL Three)     = (Player p LeftL Zero)
rotatePlayer (Player p Line Zero)       = (Player p Line One)
rotatePlayer (Player p Line One)        = (Player p Line Zero)
rotatePlayer (Player p t Zero)          = (Player p t One)
rotatePlayer (Player p t _)             = (Player p t Zero)


-- previewField is used to display the upcomming game piece.
previewField :: Field
previewField =
  [[Wall,  Wall,   Wall,   Wall,   Wall,     Wall,   Wall,   Wall]
  , [Wall, Empty,  Empty,  Empty,  Empty,    Empty,  Empty,  Wall]
  , [Wall, Empty,  Empty,  Empty,  Empty,    Empty,  Empty,  Wall]
  , [Wall, Empty,  Empty,  Empty,  Empty,    Empty,  Empty,  Wall]
  , [Wall, Empty,  Empty,  Empty,  Empty,    Empty,  Empty,  Wall]
  , [Wall, Wall,   Wall,   Wall,   Wall,     Wall,   Wall,   Wall]]


-- defaultField is used for initialization
defaultField :: Field
defaultField =
  [ [Wall, Wall,  Wall,  Wall,  Wall,    Wall,  Wall,  Wall,  Wall,  Wall,    Wall,  Wall,  Wall,  Wall,  Wall]
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
                                                                     
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
                                                                     
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
                                                                     
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
                                                                     
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
  , [Wall, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Empty,   Empty, Empty, Empty, Empty, Wall]
  , [Wall, Wall,  Wall,  Wall,  Wall,    Wall,  Wall,  Wall,  Wall,  Wall,    Wall,  Wall,  Wall,  Wall,  Wall] ]
