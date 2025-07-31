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
import Game.Actions.Commands (Command(..), CommandHandler, CommandRegistry, handleCommand, createHelpHandler)
import Game.Monad (GameM, writeLine, playerObject, scriptMap)
import Game.World.GameObjects (allObjects, getObject)
import Game.Types.Object (showRef, objName, objEnv, SomeObjectRef(..), SomeObject(..), SomeInstRef(..), objInventory, getRef, objRef, ObjectRef(..))
import Game.Mudlib.ObjectFuns (getShort)
import Game.Types.Player (Player(..))

-- | Registry of all available wizard commands
wizardCommandRegistry :: CommandRegistry
wizardCommandRegistry = Map.fromList
  [ ("here", Command cmdHere "Display information about your current environment and list objects in the room")
  , ("allobjects", Command cmdAllObjects "Display a list of all object references in the global object map")
  , ("teleport", Command cmdTeleport "Teleport to a target location")
  , ("help", Command cmdHelpHandler "Display help for available wizard commands")
  ]

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
        [target] -> do
            writeLine "You moved"
            playerTVar <- gets playerObject
            player <- liftIO $ atomically $ readTVar playerTVar
            let playerRef = (objRef (playerBase player))
            success <- move (SomeRef playerRef) (RoomRef target)
            if success
                then do
                    writeLine "Teleported!"
                    -- Update the player's base object with the new environment
                    let playerBase' = (playerBase player) { objEnv = RoomRef target }
                        updatedPlayer = player { playerBase = playerBase' }
                    -- Update the player TVar with the updated player object
                    liftIO $ atomically $ writeTVar playerTVar updatedPlayer
                else writeLine "Teleport failed."
        _ -> do
            writeLine "Huh?"

    return True

-- | Here command handler - shows information about the player's current environment
cmdHere :: CommandHandler
cmdHere _ = do
  playerTVar <- gets playerObject
  player <- liftIO $ atomically $ readTVar playerTVar
  let envRef = objEnv $ playerBase player
  
  -- Look up the room using getObject
  room <- getObject (SomeRef envRef)
  
  case room of
    Just (SomeObject roomObj) -> do
      -- Extract the room's name and display it with the reference
      let roomName = objName roomObj
      writeLine $ "\n" <> roomName <> " (" <> showRef envRef <> ")"
      
      -- Get the prototype name from the room reference
      case objRef roomObj of
        RoomRef protoName -> do
          -- Get the ScriptMap
          scriptMapTVar <- gets scriptMap
          scriptMapValue <- liftIO $ readTVarIO scriptMapTVar
          
          -- Look up the Lua state for the prototype
          case Map.lookup protoName scriptMapValue of
            Just luaState -> do
              -- Call getShort on the Lua state
              shortDesc <- liftIO $ getShort luaState
              writeLine $ "\n" <> T.pack shortDesc <> "\n"
            Nothing -> 
              writeLine $ "No script found for prototype: " <> protoName
        _ -> 
          writeLine "Not a room reference (unexpected)"
      
      -- Display the room's inventory
      let inventory = objInventory roomObj
      if null inventory
        then writeLine "The room is empty."
        else do
          writeLine "Objects in this room:"
          -- Format and display each object reference in the inventory
          mapM_ (\instRef -> case instRef of
                  SomeInstRef ir -> writeLine $ "  " <> showRef (getRef ir)) inventory
    Nothing -> do
      -- If the room is not found, just show the reference
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