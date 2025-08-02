-- MonadUtilities.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module Game.World.MonadUtilities
  ( -- Utility functions for accessing game state
    getCurrentPlayer
  , getCurrentEnvironment
  , getScriptMapValue
  , getScriptForPrototype
  -- Additional utility functions
  , displayRoomDescription
  , listObjectsInContainer
  , handleCommandResult
  ) where

import Control.Monad.State (gets, liftIO)
import Control.Concurrent.STM (atomically, readTVar, readTVarIO)
import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Game.Types.Player (Player(..))
import Game.Types.Object (objEnv, objName, objInventory, objRef, objPrototype, ObjectRef(..), SomeObjectRef(..), SomeObject(..), showRef, ObjectKind(..))
import Game.Scripts.ScriptMap (ScriptMap)
import qualified HsLua as Lua
import Game.World.GameObjects (getObject)
import Game.Mudlib.ObjectFuns (getShort)
import Game.Types.GameMonad (GameM, writeLine)
import Game.Types.CommandState (CommandState(..))

-- | Get the current player object
getCurrentPlayer :: GameM Player
getCurrentPlayer = do
  playerTVar <- gets playerObject
  liftIO $ atomically $ readTVar playerTVar

-- | Get the environment reference of the current player
getCurrentEnvironment :: GameM (ObjectRef 'RoomK)
getCurrentEnvironment = do
  player <- getCurrentPlayer
  return $ objEnv $ playerBase player

-- | Get the current script map
getScriptMapValue :: GameM ScriptMap
getScriptMapValue = do
  scriptMapTVar <- gets scriptMap
  liftIO $ readTVarIO scriptMapTVar

-- | Get the Lua state for a prototype name
getScriptForPrototype :: T.Text -> GameM (Maybe Lua.State)
getScriptForPrototype protoName = do
  scriptMapValue <- getScriptMapValue
  return $ Map.lookup protoName scriptMapValue

-- | Display a room description based on its prototype
displayRoomDescription :: ObjectRef 'RoomK -> GameM ()
displayRoomDescription envRef = do
  -- Look up the room using getObject
  room <- getObject (SomeRef envRef)
  
  case room of
    Just (SomeObject roomObj) -> do
      -- Extract the room's name and display it with the reference
      let roomName = objName roomObj
      writeLine $ "\n" <> roomName <> " (" <> showRef envRef <> ")"
      
      -- Get the prototype name from the room's objPrototype field
      let protoName = objPrototype roomObj
      -- Get the script for the prototype
      maybeScript <- getScriptForPrototype protoName
      case maybeScript of
        Just luaState -> do
          -- Call getShort on the Lua state
          shortDesc <- liftIO $ getShort luaState
          writeLine $ "\n" <> T.pack shortDesc <> "\n"
        Nothing ->
          writeLine $ "No script found for prototype: " <> protoName
    Nothing ->
      writeLine $ "Unknown room: " <> showRef envRef

-- | List objects in a container (room, inventory)
listObjectsInContainer :: SomeObject -> T.Text -> T.Text -> GameM ()
listObjectsInContainer (SomeObject objData) emptyMsg listHeaderMsg = do
  let inventory = objInventory objData
  if null inventory
    then writeLine emptyMsg
    else do
      writeLine listHeaderMsg
      -- Format and display each object reference in the inventory
      mapM_ (\ref -> writeLine $ "  " <> case ref of 
                                           SomeRef r -> showRef r) inventory

-- | Handle command result (standardized way to handle command success/failure)
handleCommandResult :: Bool -> T.Text -> T.Text -> GameM Bool
handleCommandResult success successMsg failureMsg = do
  if success
    then do
      writeLine successMsg
      return True
    else do
      writeLine failureMsg
      return False