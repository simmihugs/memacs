{-# LANGUAGE OverloadedStrings #-}
module Events.Writer.WriterEvents where

import Monomer
import Control.Lens
import Data.Text ( pack )

-- | My Modules
import Events.TetrisEvents
import Model


handleWriteEvent
  :: WidgetEnv AppModel AppEvent
  -> WidgetNode AppModel AppEvent
  -> AppModel
  -> WriteAction
  -> [AppEventResponse AppModel AppEvent]
handleWriteEvent wenv node model evt = case evt of
  -- | Start displaying textarea
  StartWriting -> [ Task $ return $ TetrisEvent TetrisStop
                  , Model $ model & displayModel .~ DisplayWriting
                  , SetFocusOnKey "fileBrowser"
                  , Task $ return $ AppendErrorMessage
                    $ pack ("WRITE_ACTION: Start writing\n")]

  -- | Stop displaying textarea
  StopWriting  -> [Model $ model & displayModel .~ DisplayNone
                  , SetFocusOnKey "startButton"
                  , Task $ return $ AppendErrorMessage
                    $ pack ("WRITE_ACTION: Stop writing\n")]
