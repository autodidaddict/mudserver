{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Types.ObjectPersistenceSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesDirectoryExist)
import System.FilePath ((</>))
import Control.Exception (bracket)
import Control.Monad (when)

import Game.Types.Object
import Game.Types.Persistable
import Game.Types.Player (mkDefaultPlayer, Player(playerBase))
import Game.Types.Room (mkDefaultRoom, Room(roomBase))

-- | Create a test ObjectData for a player
createTestObjectData :: ObjectData 'PlayerK
createTestObjectData = 
  let name = "testObject" :: Text
      ref = InstRef "std.player" "123"
      env = RoomRef "testRoom"
  in ObjectData
      { objRef = ref
      , objName = name
      , objEnv = env
      , objInventory = []
      , objVisible = Visibility 10
      , objPersistent = True
      , objPrototype = "std.player"
      }

-- | Setup and teardown for tests that need a temporary directory
withTempDirectory :: String -> (String -> IO a) -> IO a
withTempDirectory dirName action = 
  bracket
    (do
      let tempDir = "test-temp" </> dirName
      createDirectoryIfMissing True tempDir
      return tempDir)
    (\tempDir -> do
      exists <- doesDirectoryExist tempDir
      when exists $ removeDirectoryRecursive tempDir)
    action

spec :: Spec
spec = do
  describe "ObjectData persistence" $ do
    it "can save and load an ObjectData" $ do
      withTempDirectory "object-save-test" $ \tempDir -> do
        let obj = createTestObjectData
            identifier = objName obj
        
        -- Save the object
        result <- saveObject tempDir identifier obj
        result `shouldBe` Right ()
        
        -- Load the object
        loadResult <- loadObject tempDir identifier :: IO (Either String (ObjectData 'PlayerK))
        loadResult `shouldBe` Right obj

    it "returns an error when loading a non-existent object" $ do
      withTempDirectory "object-load-test" $ \tempDir -> do
        loadResult <- loadObject tempDir "nonexistentObject" :: IO (Either String (ObjectData 'PlayerK))
        case loadResult of
          Left err -> err `shouldContain` "not found"
          Right _ -> expectationFailure "Expected an error, but got an object"

    it "can handle special characters in object names" $ do
      withTempDirectory "object-special-chars-test" $ \tempDir -> do
        let name = "test-object@with.special_chars" :: Text
            ref = InstRef "std.player" "123"
            env = RoomRef "testRoom"
            obj = ObjectData ref name env [] (Visibility 10) True "std.player" :: ObjectData 'PlayerK
        
        -- Save the object
        saveResult <- saveObject tempDir name obj
        saveResult `shouldBe` Right ()
        
        -- Load the object
        loadResult <- loadObject tempDir name :: IO (Either String (ObjectData 'PlayerK))
        loadResult `shouldBe` Right obj
        
    it "preserves prototype information during serialization/deserialization" $ do
      withTempDirectory "object-prototype-test" $ \tempDir -> do
        let name = "test-object-prototype" :: Text
            ref = InstRef "std.player" "123"
            env = RoomRef "testRoom"
            obj = ObjectData ref name env [] (Visibility 10) True "std.player" :: ObjectData 'PlayerK
        
        -- Save the object
        saveResult <- saveObject tempDir name obj
        saveResult `shouldBe` Right ()
        
        -- Load the object and verify prototype is preserved
        loadResult <- loadObject tempDir name :: IO (Either String (ObjectData 'PlayerK))
        case loadResult of
          Left err -> expectationFailure $ "Failed to load object: " ++ err
          Right loadedObj -> do
            objPrototype loadedObj `shouldBe` "std.player"
            
  describe "Object prototypes" $ do
    it "creates players with std.player prototype" $ do
      let username = "testuser" :: Text
          passHash = "hashedpassword" :: Text
          startingRoom = RoomRef "startroom"
          playerRef = InstRef "std.player" "123"
          player = mkDefaultPlayer username passHash startingRoom playerRef
      
      objPrototype (playerBase player) `shouldBe` "std.player"
      objName (playerBase player) `shouldBe` username
      
    it "creates rooms with std.room prototype" $ do
      let roomProto = "dungeon" :: Text
          roomName = "Dark Dungeon" :: Text
          room = mkDefaultRoom roomProto roomName
      
      objPrototype (roomBase room) `shouldBe` "std.room"
      objName (roomBase room) `shouldBe` roomName
      
    it "uses std.object as the default prototype" $ do
      let ref = InstRef "item" "123"
          name = "Generic Object"
          env = RoomRef "testRoom"
          obj = mkObjectData ref name env [] (Visibility 10) True
      
      objPrototype obj `shouldBe` defaultPrototype
      defaultPrototype `shouldBe` "std.object"