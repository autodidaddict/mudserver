{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Game.Types.ItemSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Map.Strict as Map
import Control.Concurrent.STM
import Control.Monad.IO.Class (liftIO)
import Network.Socket (Socket)

import Game.Types.Object
import Game.Types.Player
import Game.Types.Item
import Game.Types.Common (ObjectsMap, PlayerName(..))
import Game.Monad (runGameM, CommandState(..), objectsMap)
import qualified Game.World.GameObjects as GameObjects
import Game.Scripts.ScriptMap (emptyScriptMap)

-- | Create a test room
createTestRoom :: ObjectData 'RoomK
createTestRoom =
  let name = "Test Room" :: Text
      roomRef = RoomRef "testRoom"
  in mkObjectData roomRef name roomRef [] (Visibility 10) True

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
      player = createTestPlayer roomRef
      playerObj = playerBase player
      playerRef = objRef playerObj
      pName = PlayerName "testUser"
  
  -- Add objects to the objects map
  atomically $ modifyTVar' objectsTVar $ \objMap ->
    Map.insert (SomeRef roomRef) (SomeObject roomObj) $
    Map.insert (SomeRef playerRef) (SomeObject playerObj) objMap
  
  -- Add player to the player map
  atomically $ modifyTVar' playersTVar $ \plMap ->
    Map.insert pName (socket, player) plMap
  
  -- Return the CommandState
  return $ CommandState socket pName player playersTVar objectsTVar scriptMapTVar

-- | Helper function to get the objects map from a CommandState
getObjectsMap :: CommandState -> IO ObjectsMap
getObjectsMap st = readTVarIO (objectsMap st)

-- | Helper function to check if an object exists in the objects map
objectExists :: SomeObjectRef -> ObjectsMap -> Bool
objectExists ref objMap = Map.member ref objMap

spec :: Spec
spec = do
  describe "createItem function" $ do
    it "adds the item to the global object map" $ do
      -- Create a mock socket (we won't use it for actual I/O)
      socket <- liftIO $ pure undefined
      
      -- Create a mock CommandState
      st <- liftIO $ createMockCommandState socket
      
      -- Get the initial objects map
      initialObjMap <- liftIO $ getObjectsMap st
      
      -- Create an item
      let prototype = "test.item" :: Text
          name = "Test Item" :: Text
      
      -- Run the createItem function
      item <- liftIO $ runGameM st $ createItem prototype name
      
      -- Get the updated objects map
      updatedObjMap <- liftIO $ getObjectsMap st
      
      -- Get the item reference
      let itemRef = objRef (itemBase item)
          itemSomeRef = SomeRef itemRef
      
      -- Verify the item exists in the updated map but not in the initial map
      objectExists itemSomeRef initialObjMap `shouldBe` False
      objectExists itemSomeRef updatedObjMap `shouldBe` True
      
      -- Verify we can retrieve the item from the global object map
      maybeObj <- liftIO $ runGameM st $ GameObjects.getObject itemSomeRef
      case maybeObj of
        Nothing -> expectationFailure "Item not found in global object map"
        Just (SomeObject obj) -> do
          -- Verify the item's properties
          objName obj `shouldBe` name
          objPrototype obj `shouldBe` prototype