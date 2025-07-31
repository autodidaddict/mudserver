-- CommandState.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Game.Types.CommandState
  ( CommandState(..)
  ) where

import Network.Socket (Socket)
import Control.Concurrent.STM (TVar)
import Game.Types.Player (Player, PlayerName(..), PlayerMap)
import Game.Types.Object (ObjectsMap)
import Game.Scripts.ScriptMap (ScriptMap)
import Config (ServerConfig)

-- | The state maintained for each command execution
data CommandState = CommandState
  { clientSocket :: Socket        -- ^ The client's socket connection
  , playerName   :: PlayerName    -- ^ The player's name
  , playerObject :: TVar Player   -- ^ The player object (shared)
  , playerList   :: TVar (PlayerMap Player) -- ^ Shared map of all connected players
  , objectsMap   :: TVar ObjectsMap -- ^ Shared map of all game objects
  , scriptMap    :: TVar ScriptMap -- ^ Shared map of all prototype scripts
  , serverConfig :: ServerConfig  -- ^ Copy of the server configuration (not shared)
  }