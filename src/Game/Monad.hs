-- Monad.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Game.Monad
  ( GameM
  , runGameM
  , rawWrite
  , writeLine
  , amWizard
  , CommandState(..)
  ) where

import Control.Monad.State
import qualified Network.Socket.ByteString as NSB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Game.Types.Player (playerIsWizard)
import Game.Types.CommandState (CommandState(..))

-- | The GameM monad for handling commands with state
newtype GameM a = GameM { unGameM :: StateT CommandState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState CommandState)

-- | Run a GameM with the given state
runGameM :: CommandState -> GameM a -> IO a
runGameM st action = evalStateT (unGameM action) st

-- | Send raw text to the client socket
rawWrite :: T.Text -> GameM ()
rawWrite msg = do
  sock <- gets clientSocket
  liftIO $ NSB.sendAll sock (TE.encodeUtf8 msg)

-- | Send a line of text to the client (with CRLF)
writeLine :: T.Text -> GameM ()
writeLine s = rawWrite (s `T.append` "\r\n")

-- | Check if the current player has wizard privileges
amWizard :: GameM Bool
amWizard = do
  player <- gets playerObject
  return $ playerIsWizard player