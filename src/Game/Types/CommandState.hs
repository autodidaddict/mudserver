-- CommandState.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Game.Types.CommandState
  ( CommandState(..)
  ) where

import Network.Socket (Socket)
import Control.Concurrent.STM (TVar)
import Game.Types.Player (Player)
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