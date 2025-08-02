{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Game.MonadSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Network.Socket (Socket)

import Game.Types.Object
import Game.Types.Player
import Game.Types.Player(PlayerName(..))
import Game.Monad (runGameM, CommandState(..), objectsMap)
import Game.World.GameObjects (deleteObject)
import Game.Actions.Inventory (addToInventory)
import Game.Scripts.ScriptMap (emptyScriptMap)
import Config (ServerConfig, defaultConfig)

-- | Create a test room
createTestRoom :: ObjectData 'RoomK
createTestRoom =
  let name = "Test Room" :: Text
      roomRef = RoomRef "testRoom"
  in mkObjectData roomRef name roomRef [] (Visibility 10) True

-- | Create a test item
createTestItem :: ObjectRef 'RoomK -> ObjectData 'ItemK
createTestItem envRef =
  let name = "Test Item" :: Text
      itemRef = InstRef "item" "123"
  in mkObjectData itemRef name envRef [] (Visibility 10) True

-- | Create a test player
createTestPlayer :: ObjectRef 'RoomK -> Player
createTestPlayer envRef =
  let username = "testUser" :: Text
      passHash = "hashedPassword123" :: Text
      playerRef = InstRef "std.player" "456"
  in mkDefaultPlayer username passHash envRef playerRef

-- | Create a mock CommandState for testing
createMockCommandState :: Socket -> IO CommandState
createMockCommandState socket = do
  -- Create empty TVars for player map, objects map, and script map
  playersTVar <- newTVarIO Map.empty
  objectsTVar <- newTVarIO Map.empty
  scriptMapTVar <- newTVarIO emptyScriptMap
  
  -- Create test objects
  let roomObj = createTestRoom
      roomRef = objRef roomObj
      itemObj = createTestItem roomRef
      itemRef = objRef itemObj
      player = createTestPlayer roomRef
      playerObj = playerBase player
      playerRef = objRef playerObj
      pName = PlayerName "testUser"
  
  -- Add objects to the objects map
  atomically $ modifyTVar' objectsTVar $ \objMap ->
    Map.insert (SomeRef roomRef) (SomeObject roomObj) $
    Map.insert (SomeRef itemRef) (SomeObject itemObj) $
    Map.insert (SomeRef playerRef) (SomeObject playerObj) objMap
  
  -- Add player to the player map
  atomically $ modifyTVar' playersTVar $ \plMap ->
    Map.insert pName (socket, player) plMap
  
  -- Add item and player to room's inventory
  -- Use item and player references directly
  let updatedRoomObj = addToInventory (addToInventory roomObj itemRef) playerRef
  atomically $ modifyTVar' objectsTVar $ \objMap ->
    Map.insert (SomeRef roomRef) (SomeObject updatedRoomObj) objMap
  
  -- Create a TVar for the Player object
  playerTVar <- newTVarIO player
  
  -- Return the CommandState with default config
  return $ CommandState socket pName playerTVar playersTVar objectsTVar scriptMapTVar defaultConfig

-- | Helper function to get the objects map from a CommandState
getObjectsMap :: CommandState -> IO ObjectsMap
getObjectsMap st = readTVarIO (objectsMap st)

-- | Helper function to check if an object exists in the objects map
objectExists :: SomeObjectRef -> ObjectsMap -> Bool
objectExists ref objMap = Map.member ref objMap

-- | Helper function to get an object from the objects map
getObjectFromMap :: SomeObjectRef -> ObjectsMap -> Maybe SomeObject
getObjectFromMap ref objMap = Map.lookup ref objMap

-- | Helper function to check if an object is in a room's inventory
objectInRoomInventory :: SomeObjectRef -> SomeObject -> Bool
objectInRoomInventory ref (SomeObject obj) = 
  case obj of
    roomObj -> ref `elem` objInventory roomObj

spec :: Spec
spec = do
  describe "deleteObject function" $ do
    it "never removes rooms from the global object map" $ do
      -- This test is a bit artificial since we can't actually call deleteObject on a room
      -- due to type constraints, but we'll verify that rooms remain in the map
      
      -- Create a mock socket (we won't use it for actual I/O)
      socket <- liftIO $ pure undefined
      
      -- Create a mock CommandState
      st <- liftIO $ createMockCommandState socket
      
      -- Get the initial objects map
      initialObjMap <- liftIO $ getObjectsMap st
      
      -- Find the room reference
      let roomRef = SomeRef (RoomRef "testRoom")
      
      -- Verify the room exists in the initial map
      objectExists roomRef initialObjMap `shouldBe` True
      
      -- Run the deleteObject function on the room reference
      -- This should be a no-op for rooms
      _ <- liftIO $ runGameM st $ deleteObject roomRef
      
      -- Get the updated objects map
      updatedObjMap <- liftIO $ getObjectsMap st
      
      -- Verify the room still exists in the map
      objectExists roomRef updatedObjMap `shouldBe` True
    
    it "removes items from the global object map" $ do
      -- Create a mock socket
      socket <- liftIO $ pure undefined
      
      -- Create a mock CommandState
      st <- liftIO $ createMockCommandState socket
      
      -- Get the initial objects map
      initialObjMap <- liftIO $ getObjectsMap st
      
      -- Find the item reference
      let itemRef = SomeRef (InstRef "item" "123")
      
      -- Verify the item exists in the initial map
      objectExists itemRef initialObjMap `shouldBe` True
      
      -- Run the deleteObject function on the item reference
      _ <- liftIO $ runGameM st $ deleteObject itemRef
      
      -- Get the updated objects map
      updatedObjMap <- liftIO $ getObjectsMap st
      
      -- Verify the item no longer exists in the map
      objectExists itemRef updatedObjMap `shouldBe` False
    
    it "removes items from their environment's inventory" $ do
      -- Create a mock socket
      socket <- liftIO $ pure undefined
      
      -- Create a mock CommandState
      st <- liftIO $ createMockCommandState socket
      
      -- Get the initial objects map
      initialObjMap <- liftIO $ getObjectsMap st
      
      -- Find the room and item references
      let roomRef = SomeRef (RoomRef "testRoom")
          itemRef = SomeRef (InstRef "item" "123")
      
      -- Get the room object
      let roomObj = case getObjectFromMap roomRef initialObjMap of
                      Just obj -> obj
                      Nothing -> error "Room object not found in initial map"
      
      -- Verify the item is in the room's inventory
      objectInRoomInventory itemRef roomObj `shouldBe` True
      
      -- Run the deleteObject function on the item reference
      _ <- liftIO $ runGameM st $ deleteObject itemRef
      
      -- Get the updated objects map
      updatedObjMap <- liftIO $ getObjectsMap st
      
      -- Get the updated room object
      let updatedRoomObj = case getObjectFromMap roomRef updatedObjMap of
                             Just obj -> obj
                             Nothing -> error "Room object not found in updated map"
      
      -- Verify the item is no longer in the room's inventory
      objectInRoomInventory itemRef updatedRoomObj `shouldBe` False
    
    it "removes players from the global object map" $ do
      -- Create a mock socket
      socket <- liftIO $ pure undefined
      
      -- Create a mock CommandState
      st <- liftIO $ createMockCommandState socket
      
      -- Get the initial objects map
      initialObjMap <- liftIO $ getObjectsMap st
      
      -- Find the player reference
      let playerRef = SomeRef (InstRef "std.player" "456")
      
      -- Verify the player exists in the initial map
      objectExists playerRef initialObjMap `shouldBe` True
      
      -- Run the deleteObject function on the player reference
      _ <- liftIO $ runGameM st $ deleteObject playerRef
      
      -- Get the updated objects map
      updatedObjMap <- liftIO $ getObjectsMap st
      
      -- Verify the player no longer exists in the map
      objectExists playerRef updatedObjMap `shouldBe` False
    
    it "removes players from their environment's inventory" $ do
      -- Create a mock socket
      socket <- liftIO $ pure undefined
      
      -- Create a mock CommandState
      st <- liftIO $ createMockCommandState socket
      
      -- Get the initial objects map
      initialObjMap <- liftIO $ getObjectsMap st
      
      -- Find the room and player references
      let roomRef = SomeRef (RoomRef "testRoom")
          playerRef = SomeRef (InstRef "std.player" "456")
      
      -- Get the room object
      let roomObj = case getObjectFromMap roomRef initialObjMap of
                      Just obj -> obj
                      Nothing -> error "Room object not found in initial map"
      
      -- Verify the player is in the room's inventory
      objectInRoomInventory playerRef roomObj `shouldBe` True
      
      -- Run the deleteObject function on the player reference
      _ <- liftIO $ runGameM st $ deleteObject playerRef
      
      -- Get the updated objects map
      updatedObjMap <- liftIO $ getObjectsMap st
      
      -- Get the updated room object
      let updatedRoomObj = case getObjectFromMap roomRef updatedObjMap of
                             Just obj -> obj
                             Nothing -> error "Room object not found in updated map"
      
      -- Verify the player is no longer in the room's inventory
      objectInRoomInventory playerRef updatedRoomObj `shouldBe` False