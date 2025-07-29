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
import Network.Socket (Socket)
import Control.Concurrent.STM (TVar)
import qualified Network.Socket.ByteString as NSB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Game.Types.Player (Player, playerIsWizard)
import Game.Types.Common (PlayerName(..), PlayerMap, ObjectsMap)
import Game.Scripts.ScriptMap (ScriptMap)

-- | The state maintained for each command execution
data CommandState = CommandState
  { clientSocket :: Socket        -- ^ The client's socket connection
  , playerName   :: PlayerName    -- ^ The player's name
  , playerObject :: Player        -- ^ The player object
  , playerList   :: TVar (PlayerMap Player) -- ^ Shared map of all connected players
  , objectsMap   :: TVar ObjectsMap -- ^ Shared map of all game objects
  , scriptMap    :: TVar ScriptMap -- ^ Shared map of all prototype scripts
  }

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