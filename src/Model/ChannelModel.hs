module Model.ChannelModel where

import Control.Lens
import Data.Text (Text)
import Data.Time
import Monomer
import Control.Concurrent.STM.TChan


data GameState = NotRunning | Running | Restart deriving (Eq, Show)

-- Show instance needed as by Monomer for main Appmodel
instance Show (TChan a) where
  show _ = "d"

data ChannelModel = ChannelModel {
  _cmQueue            :: (TChan Int),
  _cmGameState        :: (TChan GameState)
  } deriving (Eq, Show)

makeLensesWith abbreviatedFields ''ChannelModel
