-- WizardCommands.hs
{-# LANGUAGE OverloadedStrings #-}

module Game.Actions.WizardCommands
  ( handleWizardCommand
  , Command
  , CommandHandler
  , wizardCommandRegistry
  ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Control.Monad.State (gets, liftIO)
import Control.Concurrent.STM (readTVarIO, atomically, readTVar, writeTVar)
import Game.World.Movement (move)
import Game.Actions.Commands (Command(..), CommandHandler, CommandRegistry, handleCommand, createHelpHandler, registerCommand)
import Game.Monad (GameM, writeLine, playerObject, getCurrentPlayer, getCurrentEnvironment, 
                  displayRoomDescription, listObjectsInContainer, handleCommandResult)
import Game.World.GameObjects (allObjects, getObject)
import Game.Types.Object (showRef, objName, SomeObjectRef(..), SomeObject(..), objRef, objEnv, ObjectRef(..))
import Game.Types.Player (Player(..))

-- | Registry of all available wizard commands
wizardCommandRegistry :: CommandRegistry
wizardCommandRegistry = 
  let emptyRegistry = Map.empty
      hereCmd = Command { cmdHandler = cmdHere, cmdHelp = "Display information about your current environment and list objects in the room", cmdPrimary = "" }
      allObjectsCmd = Command { cmdHandler = cmdAllObjects, cmdHelp = "Display a list of all object references in the global object map", cmdPrimary = "" }
      teleportCmd = Command { cmdHandler = cmdTeleport, cmdHelp = "Teleport to a target location", cmdPrimary = "" }
      helpCmd = Command { cmdHandler = cmdHelpHandler, cmdHelp = "Display help for available wizard commands", cmdPrimary = "" }
  in registerCommand ["here", "where"] hereCmd $
     registerCommand ["allobjects", "objects", "objs"] allObjectsCmd $
     registerCommand ["teleport", "tp", "goto"] teleportCmd $
     registerCommand ["help"] helpCmd emptyRegistry

-- | Handle wizard commands (commands that start with '@')
-- These commands are only available to players with the isWizard flag
handleWizardCommand :: T.Text -> GameM Bool
handleWizardCommand = handleCommand wizardCommandRegistry "@"

-- | Teleport command handler - moves the wizard to the specified target
cmdTeleport :: CommandHandler
cmdTeleport args = do
    case args of
        [] -> do
            writeLine "Usage: @teleport <script path>"
            return True
        [target] -> do
            -- Get the current player
            player <- getCurrentPlayer
            let playerRef = objRef (playerBase player)
            
            -- Attempt to move the player
            success <- move (SomeRef playerRef) (RoomRef target)
            
            if success
                then do
                    -- Update the player's base object with the new environment
                    let playerBase' = (playerBase player) { objEnv = RoomRef target }
                        updatedPlayer = player { playerBase = playerBase' }
                    -- Update the player TVar with the updated player object
                    playerTVar <- gets playerObject
                    liftIO $ atomically $ writeTVar playerTVar updatedPlayer
                    
                    -- Handle the successful result
                    handleCommandResult True "Teleported!" ""
                else 
                    -- Handle the failed result
                    handleCommandResult False "" "Teleport failed."
        _ -> do
            writeLine "Huh?"
            return True

-- | Here command handler - shows information about the player's current environment
cmdHere :: CommandHandler
cmdHere _ = do
  -- Get the current environment reference
  envRef <- getCurrentEnvironment
  
  -- Display the room description
  displayRoomDescription envRef
  
  -- Look up the room using getObject
  room <- getObject (SomeRef envRef)
  
  -- Display objects in the room if found
  case room of
    Just someObj@(SomeObject roomObj) -> 
      listObjectsInContainer someObj "The room is empty." "Objects in this room:"
    Nothing -> 
      writeLine $ "Unknown room: " <> showRef envRef
  
  return True

-- | AllObjects command handler - shows a list of all object references in the global object map
cmdAllObjects :: CommandHandler
cmdAllObjects _ = do
  objMapTVar <- allObjects
  objMap <- liftIO $ readTVarIO objMapTVar
  
  writeLine "All objects in the global object map:"
  writeLine ""
  
  -- Format and display each object reference
  mapM_ (\(SomeRef ref) -> writeLine $ "  " <> showRef ref) (Map.keys objMap)

  writeLine ""
  writeLine $ "Total objects: " <> T.pack (show (Map.size objMap))
  
  return True

cmdHelpHandler :: CommandHandler
cmdHelpHandler = createHelpHandler wizardCommandRegistry "@" "Available wizard commands:"