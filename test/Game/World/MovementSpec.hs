{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.World.MovementSpec (spec) where

import Test.Hspec
import Control.Monad.State (evalStateT)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (newTVarIO, readTVarIO)
import qualified Data.Map.Strict as Map
import Network.Socket (Socket)
import Data.Text (Text)

import Game.Types.Object
import Game.Types.Player
import Game.Types.CommandState
import Game.Monad
import Game.World.Movement
import Game.World.GameObjects
import Config (ServerConfig, defaultConfig)

-- | Create a test item
createTestItem :: ObjectData 'ItemK
createTestItem =
  let name = "testItem" :: Text
      obRef = InstRef "item" "123"
      env = RoomRef "sourceRoom"
  in mkObjectData obRef name env [] (Visibility 10) True

-- | Create a test player
createTestPlayer :: Player
createTestPlayer =
  let username = "testUser" :: Text
      passHash = "hashedPassword123" :: Text
      startingRoom = RoomRef "sourceRoom"
      playerRef = InstRef "std.player" "456"
  in mkDefaultPlayer username passHash startingRoom playerRef

-- | Create a test source room
createSourceRoom :: ObjectData 'RoomK
createSourceRoom =
  let name = "Source Room" :: Text
      obRef = RoomRef "sourceRoom"
      env = RoomRef "sourceRoom" -- Room is its own environment
  in mkObjectData obRef name env [] (Visibility 10) True

-- | Create a test destination room
createDestRoom :: ObjectData 'RoomK
createDestRoom =
  let name = "Destination Room" :: Text
      obRef = RoomRef "destRoom"
      env = RoomRef "destRoom" -- Room is its own environment
  in mkObjectData obRef name env [] (Visibility 10) True

-- | Create a test command state with the given objects
createTestCommandState :: Socket -> Map.Map SomeObjectRef SomeObject -> IO CommandState
createTestCommandState sock objects = do
  objMapTVar <- newTVarIO objects
  playerMapTVar <- newTVarIO Map.empty
  scriptMapTVar <- newTVarIO Map.empty
  let player = createTestPlayer
      playerName = PlayerName "testUser"
  
  -- Create a TVar for the player
  playerTVar <- newTVarIO player
  
  return $ CommandState sock playerName playerTVar playerMapTVar objMapTVar scriptMapTVar defaultConfig

-- | Run a GameM action with the given objects
runWithObjects :: Socket -> Map.Map SomeObjectRef SomeObject -> GameM a -> IO a
runWithObjects sock objects action = do
  cmdState <- createTestCommandState sock objects
  runGameM cmdState action

-- | Mock socket for testing
mockSocket :: Socket
mockSocket = error "This socket should never be used"

spec :: Spec
spec = do
  describe "move function" $ do
    it "successfully moves an object from one environment to another" $ do
      -- Create test objects
      let sourceRoom = createSourceRoom
          destRoom = createDestRoom
          item = createTestItem
          
          -- Create object references
          sourceRoomRef = objRef sourceRoom
          destRoomRef = objRef destRoom
          itemRef = SomeRef (objRef item)
          
          -- Create initial objects map
          initialObjects = Map.fromList
            [ (SomeRef sourceRoomRef, SomeObject sourceRoom)
            , (SomeRef destRoomRef, SomeObject destRoom)
            , (itemRef, SomeObject item)
            ]
      
      -- Run the move function
      result <- runWithObjects mockSocket initialObjects $ do
        -- First add the item to the source room's inventory
        _ <- addObjectToEnvironment itemRef
        -- Then move the item to the destination room
        move itemRef destRoomRef
      
      -- Verify that the move was successful
      result `shouldBe` True
    
    it "returns False when the object doesn't exist" $ do
      -- Create test objects
      let sourceRoom = createSourceRoom
          destRoom = createDestRoom
          
          -- Create object references
          sourceRoomRef = objRef sourceRoom
          destRoomRef = objRef destRoom
          nonExistentItemRef = SomeRef (InstRef "item" "nonexistent" :: ObjectRef 'ItemK)
          
          -- Create initial objects map
          initialObjects = Map.fromList
            [ (SomeRef sourceRoomRef, SomeObject sourceRoom)
            , (SomeRef destRoomRef, SomeObject destRoom)
            ]
      
      -- Run the move function with a non-existent item
      result <- runWithObjects mockSocket initialObjects $ 
        move nonExistentItemRef destRoomRef
      
      -- Verify that the move failed
      result `shouldBe` False
    
    it "verifies that the destination environment is loaded when it doesn't exist" $ do
      -- Create test objects
      let sourceRoom = createSourceRoom
          item = createTestItem
          
          -- Create object references
          sourceRoomRef = objRef sourceRoom
          nonExistentRoomRef = RoomRef "nonexistent"
          itemRef = SomeRef (objRef item)
          
          -- Create initial objects map
          initialObjects = Map.fromList
            [ (SomeRef sourceRoomRef, SomeObject sourceRoom)
            , (itemRef, SomeObject item)
            ]
      
      -- Run the move function with a non-existent destination
      (result, objMap) <- runWithObjects mockSocket initialObjects $ do
        -- First add the item to the source room's inventory
        _ <- addObjectToEnvironment itemRef
        -- Then try to move the item to a non-existent room
        moveResult <- move itemRef nonExistentRoomRef
        -- Get the updated objects map to verify the environment was loaded
        objMapTVar <- allObjects
        objMap <- liftIO $ readTVarIO objMapTVar
        return (moveResult, objMap)
      
      -- Verify that the move was successful
      result `shouldBe` True
      
      -- Verify that the destination environment was loaded
      Map.member (SomeRef nonExistentRoomRef) objMap `shouldBe` True