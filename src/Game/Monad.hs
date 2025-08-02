-- Monad.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Game.Monad
  ( -- Re-export GameM and basic functions from Game.Types.GameMonad
    GameM(..)
  , runGameM
  , rawWrite
  , writeLine
  -- Additional functions
  , amWizard
  , getCommandState
  , putCommandState
  , CommandState(..)
  -- Re-export utility functions from Game.World.MonadUtilities
  , getCurrentPlayer
  , getCurrentEnvironment
  , getScriptMapValue
  , getScriptForPrototype
  , displayRoomDescription
  , listObjectsInContainer
  , handleCommandResult
  ) where

import Control.Monad.State (get, put, gets)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (atomically, readTVar)
import Game.Types.Player (playerIsWizard)
import Game.Types.CommandState (CommandState(..))
-- Import GameM type and basic functions
import Game.Types.GameMonad (GameM(..), runGameM, rawWrite, writeLine)
-- Re-export utility functions
import Game.World.MonadUtilities (
    getCurrentPlayer
  , getCurrentEnvironment
  , getScriptMapValue
  , getScriptForPrototype
  , displayRoomDescription
  , listObjectsInContainer
  , handleCommandResult
  )

-- | Check if the current player has wizard privileges
amWizard :: GameM Bool
amWizard = do
  playerTVar <- gets playerObject
  player <- liftIO $ atomically $ readTVar playerTVar
  return $ playerIsWizard player

-- | Get the current command state
getCommandState :: GameM CommandState
getCommandState = get

-- | Set the command state
putCommandState :: CommandState -> GameM ()
putCommandState = put