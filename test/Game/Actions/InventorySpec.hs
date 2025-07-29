{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Actions.InventorySpec (spec) where

import Test.Hspec
import Data.Text (Text)

import Game.Types.Object
import Game.Types.Player
import Game.Actions.Inventory

-- | Create a test item
createTestItem :: ObjectData 'ItemK
createTestItem =
  let name = "testItem" :: Text
      obRef = InstRef "item" "123"
      env = RoomRef "testRoom"
  in mkObjectData obRef name env [] (Visibility 10) True

-- | Create a test player
createTestPlayer :: Player
createTestPlayer =
  let username = "testUser" :: Text
      passHash = "hashedPassword123" :: Text
      startingRoom = RoomRef "startRoom"
      playerRef = InstRef "std.player" "456"
  in mkDefaultPlayer username passHash startingRoom playerRef

spec :: Spec
spec = do
  describe "Inventory type constraints" $ do
    it "allows adding instantiable objects (items) to inventory" $ do
      let item = createTestItem
          player = createTestPlayer
          playerObj = playerBase player
          
      -- Create an instanced reference to the item
      case mkInstancedRef (objRef item) of
        Nothing -> expectationFailure "Failed to create instanced reference for item"
        Just itemRef -> do
          -- Add the item to the player's inventory
          let updatedPlayerObj = addToInventory playerObj itemRef
          
          -- Verify that the item was added to the inventory
          length (objInventory updatedPlayerObj) `shouldBe` 1
    
    it "documents that rooms cannot be added to inventory due to type constraints" $ do
      -- This is a documentation test to explain why rooms cannot be added to inventory
      
      -- The following code would not compile because rooms are not instantiable:
      -- 
      -- let roomRef = RoomRef "someRoom"
      -- case mkInstancedRef roomRef of
      --   Nothing -> ... -- This would always be the case
      --   Just roomInstRef -> addToInventory playerObj roomInstRef
      --
      -- The error would be:
      -- • No instance for (IsInstantiable 'RoomK)
      --   arising from a use of 'addToInventory'
      
      -- Instead, we verify that mkInstancedRef returns Nothing for room references
      let roomRef = RoomRef "someRoom"
      case mkInstancedRef roomRef of
        Nothing -> return () -- Expected: rooms cannot be instantiated
        Just _ -> expectationFailure "Room references should not be instantiable"
      
      -- This test passes because rooms cannot be instantiated, which is the first
      -- barrier preventing them from being added to inventories. Even if we could
      -- somehow create an InstancedRef 'RoomK, the IsInstantiable constraint on
      -- addToInventory would prevent it from being added to an inventory.
      
      -- The type system enforces that:
      -- 1. Rooms cannot be instantiated (no IsInstantiable instance for 'RoomK)
      -- 2. Only instantiable objects can be added to inventories (IsInstantiable constraint)
  
  describe "Inventory management" $ do
    it "can remove objects from inventory" $ do
      let item = createTestItem
          player = createTestPlayer
          playerObj = playerBase player
          
      -- Create an instanced reference to the item
      case mkInstancedRef (objRef item) of
        Nothing -> expectationFailure "Failed to create instanced reference for item"
        Just itemRef -> do
          -- Add the item to the player's inventory
          let playerWithItem = addToInventory playerObj itemRef
          
          -- Verify that the item was added to the inventory
          length (objInventory playerWithItem) `shouldBe` 1
          
          -- Remove the item from the player's inventory
          let playerWithoutItem = removeFromInventory playerWithItem itemRef
          
          -- Verify that the item was removed from the inventory
          length (objInventory playerWithoutItem) `shouldBe` 0
    
    it "handles removing objects that aren't in the inventory" $ do
      let item = createTestItem
          player = createTestPlayer
          playerObj = playerBase player
          
      -- Create an instanced reference to the item
      case mkInstancedRef (objRef item) of
        Nothing -> expectationFailure "Failed to create instanced reference for item"
        Just itemRef -> do
          -- Remove the item from the player's inventory (even though it's not there)
          let updatedPlayerObj = removeFromInventory playerObj itemRef
          
          -- Verify that the inventory is still empty
          length (objInventory updatedPlayerObj) `shouldBe` 0
    
    it "can remove objects from environment (room)" $ do
      -- Create a test room
      let roomRef = RoomRef "testRoom"
          roomObj = mkObjectData roomRef "Test Room" roomRef [] (Visibility 10) True
          player = createTestPlayer
          playerObj = playerBase player
      
      -- Create an instanced reference to the player
      case mkInstancedRef (objRef playerObj) of
        Nothing -> expectationFailure "Failed to create instanced reference for player"
        Just playerRef -> do
          -- Add the player to the room's inventory
          let roomWithPlayer = addToInventory roomObj playerRef
          
          -- Verify that the player was added to the room's inventory
          length (objInventory roomWithPlayer) `shouldBe` 1
          
          -- Remove the player from the room's inventory
          let roomWithoutPlayer = removeFromInventory roomWithPlayer playerRef
          
          -- Verify that the player was removed from the room's inventory
          length (objInventory roomWithoutPlayer) `shouldBe` 0