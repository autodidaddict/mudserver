-- GameObjects.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.World.GameObjects
  ( allObjects
  , deleteObject
  , getObject
  , addObject
  , addObjectToEnvironment
  , removeObjectFromEnvironment
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets)
import Control.Concurrent.STM (TVar, readTVarIO, atomically, modifyTVar')
import qualified Control.Monad as M
import qualified Data.Map.Strict as Map

import Game.Types.Object (getProto, SomeObjectRef(..), SomeObject(..), ObjectRef(..), ObjectData(..), ObjectKind(..), ObjectsMap, objEnv, IsInstantiable)
import Game.Types.GameMonad (GameM)
import Game.Types.CommandState (CommandState(..))
import Game.Actions.Inventory (addToInventory, removeFromInventory)
import Game.Mudlib.ObjectFuns (notifyEnteredInv)
import Game.Scripts.Lua (getLuaState)
import Game.Types.Room (Room(..), mkDefaultRoom)

-- | Get the map of all game objects
allObjects :: GameM (TVar ObjectsMap)
allObjects = gets (\s -> objectsMap s)

-- | Delete an object from the global object map
-- If the object's environment (a room) can be located, remove it from the room's inventory
deleteObject :: SomeObjectRef -> GameM ()
deleteObject ref = do
  -- Remove from environment if possible
  _ <- removeObjectFromEnvironment ref
  
  -- Remove from global object map (except for rooms)
  case ref of
    SomeRef (RoomRef _) -> return () -- Never remove rooms
    _ -> do
      objMapTVar <- allObjects
      liftIO $ atomically $ modifyTVar' objMapTVar (Map.delete ref)

-- | Look up an object by its reference
-- Returns Nothing if the object doesn't exist in the allObjects map
getObject :: SomeObjectRef -> GameM (Maybe SomeObject)
getObject ref = do
  objMapTVar <- allObjects
  objMap <- liftIO $ readTVarIO objMapTVar
  return $ Map.lookup ref objMap

-- | Add an object to the global objects map
addObject :: ObjectRef k -> ObjectData k -> GameM ()
addObject ref obj = do
  objMapTVar <- allObjects
  liftIO $ atomically $ modifyTVar' objMapTVar (Map.insert (SomeRef ref) (SomeObject obj))

-- | Helper function to handle instantiable objects in environment operations
handleInstantiableObject :: (IsInstantiable k) => 
                           ObjectRef k -> 
                           ObjectData 'RoomK -> 
                           (ObjectData 'RoomK -> ObjectRef k -> ObjectData 'RoomK) -> 
                           SomeObjectRef -> 
                           ObjectRef 'RoomK -> 
                           GameM Bool
handleInstantiableObject objRefParam roomObj updateFn ref envRef = do
  -- Add/remove the object to/from the room's inventory
  let updatedRoomObj = updateFn roomObj objRefParam
  
  -- Update the room in the global object map
  objMapTVar <- allObjects
  liftIO $ atomically $ modifyTVar' objMapTVar 
    (Map.insert (SomeRef envRef) (SomeObject updatedRoomObj))
  
  -- For add operations, notify that the object entered the inventory
  -- Use a simple flag to determine if this is an add operation
  let isAddOperation = case objInventory updatedRoomObj of
        newInv | SomeRef objRefParam `elem` newInv -> True  -- Object is in inventory after update
        _ -> False
  
  M.when isAddOperation $ do
    let proto = getProto envRef
    luaState <- getLuaState proto
    _ <- liftIO $ notifyEnteredInv ref envRef luaState
    return ()
    
  return True

-- | Add an object to its environment
-- This function:
-- 1. Gets the object's environment reference
-- 2. Gets the environment object from the global objects map
-- 3. Adds the object to the environment's inventory
-- 4. Updates the environment in the global objects map
addObjectToEnvironment :: SomeObjectRef -> GameM Bool
addObjectToEnvironment ref = do
  -- Get the object
  maybeObj <- getObject ref
  
  -- Early return if object doesn't exist
  case maybeObj of
    Nothing -> return False
    Just (SomeObject obj) -> do
      -- Get the object's environment reference
      let envRef = objEnv obj
      let proto = getProto envRef

      -- Get the environment object
      maybeEnvObj <- getObject (SomeRef envRef)
      
      -- If environment doesn't exist, add it to the global object map and load its script
      case maybeEnvObj of
        Nothing -> do
          let roomName = proto  -- Using the prototype name as the room name for simplicity
              defaultRoom = mkDefaultRoom proto roomName
              roomData = roomBase defaultRoom
          addObject envRef roomData
          return True

        Just (SomeObject roomObj) -> do
          -- We need to ensure roomObj is treated as ObjectData 'RoomK
          let roomObjAsRoom = case objRef roomObj of
                RoomRef _ -> roomObj
                _ -> error "Expected a room object"
          -- Handle different object types based on their reference
          case ref of
            -- Handle player objects
            SomeRef (InstRef "std.player" objId) -> 
              -- Use a type assertion for player objects
              let playerRef = InstRef "std.player" objId :: ObjectRef 'PlayerK
              in handleInstantiableObject playerRef roomObjAsRoom addToInventory ref envRef
                
            -- Handle item objects
            SomeRef (InstRef protoName objId) -> 
              -- Use a type assertion for item objects
              let itemRef = InstRef protoName objId :: ObjectRef 'ItemK
              in handleInstantiableObject itemRef roomObjAsRoom addToInventory ref envRef
                
            _ -> return False -- Not an instantiable object

-- | Remove an object from its environment
-- This function:
-- 1. Gets the object's environment reference
-- 2. Gets the environment object from the global objects map
-- 3. Removes the object from the environment's inventory
-- 4. Updates the environment in the global objects map
removeObjectFromEnvironment :: SomeObjectRef -> GameM Bool
removeObjectFromEnvironment ref = do
  -- Get the object
  maybeObj <- getObject ref
  
  -- Early return if object doesn't exist
  case maybeObj of
    Nothing -> return False
    Just (SomeObject obj) -> do
      -- Get the object's environment reference
      let envRef = objEnv obj
      
      -- Get the environment object
      maybeEnvObj <- getObject (SomeRef envRef)
      
      -- Early return if environment doesn't exist
      case maybeEnvObj of
        Nothing -> return False
        Just (SomeObject roomObj) -> do
          -- We need to ensure roomObj is treated as ObjectData 'RoomK
          let roomObjAsRoom = case objRef roomObj of
                RoomRef _ -> roomObj
                _ -> error "Expected a room object"
          -- Handle different object types based on their reference
          case ref of
            -- Handle player objects
            SomeRef (InstRef "std.player" objId) -> 
              -- Use a type assertion for player objects
              let playerRef = InstRef "std.player" objId :: ObjectRef 'PlayerK
              in handleInstantiableObject playerRef roomObjAsRoom removeFromInventory ref envRef
                
            -- Handle item objects
            SomeRef (InstRef protoName objId) -> 
              -- Use a type assertion for item objects
              let itemRef = InstRef protoName objId :: ObjectRef 'ItemK
              in handleInstantiableObject itemRef roomObjAsRoom removeFromInventory ref envRef
                
            _ -> return False -- Not an instantiable object
