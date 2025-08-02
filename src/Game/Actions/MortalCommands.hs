{-# LANGUAGE OverloadedStrings #-}

module Game.Actions.MortalCommands
  ( handleMortalCommand
  , Command
  , CommandHandler
  , commandRegistry
  ) where

import Game.Actions.Commands (Command(..), CommandHandler, CommandRegistry, handleCommand, createHelpHandler)
import Game.Monad (GameM, playerObject, writeLine, playerList, playerName, scriptMap)
import Game.Types.Object (showRef, objName, objEnv, SomeObjectRef(..), SomeObject(..), SomeInstRef(..), objInventory, getRef, objRef, ObjectRef(..))
import Game.World.GameObjects (getObject)
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class (liftIO)
import qualified Data.Text as T
import Control.Concurrent.STM (readTVar, atomically, readTVarIO)
import Control.Monad.State (gets)
import Game.Types.Player (Player(..))
import Game.Mudlib.ObjectFuns (getShort)
import Game.Types.Room


-- | Registry of all available mortal commands
commandRegistry :: CommandRegistry
commandRegistry = Map.fromList
  [ ("look", Command cmdExamine "Examine (look at) some object or the current environment")
  , ("help", Command cmdHelpHandler "Display help for available commands")
  ]

-- | Handle mortal commands (no prefix)
handleMortalCommand :: T.Text -> GameM Bool
handleMortalCommand = handleCommand commandRegistry ""

-- | Quit command handler
cmdExamine :: CommandHandler
cmdExamine args = do
  playerTVar <- gets playerObject
  player <- liftIO $ atomically $ readTVar playerTVar
  let envRef = objEnv $ playerBase player

  -- Look up the room using getObject
  room <- getObject (SomeRef envRef)

  case room of
    Just (SomeObject roomObj) -> do
      -- Extract the room's name and display it with the reference

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
    Nothing -> do
      writeLine "Unknown room"

  return True

-- | Help command handler
cmdHelpHandler :: CommandHandler
cmdHelpHandler = createHelpHandler commandRegistry "" "Available commands:"
