{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Types.PlayerPersistenceSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive, doesDirectoryExist)
import System.FilePath ((</>))
import Control.Exception (bracket)
import Control.Monad (when)
import Data.Aeson (encode, decode)

import Game.Types.Player
import Game.Types.Object

-- | Create a test player
createTestPlayer :: Player
createTestPlayer = 
  let username = "testUser" :: Text
      passHash = "hashedPassword123" :: Text
      startingRoom = RoomRef "startRoom"
      playerRef = InstRef "std.player" "123"
  in mkDefaultPlayer username passHash startingRoom playerRef

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
  describe "Player JSON serialization" $ do
    let player = createTestPlayer

    it "can serialize and deserialize a player" $ do
      let jsonData = encode player
          decodedPlayer = decode jsonData :: Maybe Player
      decodedPlayer `shouldBe` Just player
      
    it "can serialize and deserialize a player with wizard status" $ do
      let wizardPlayer = (createTestPlayer) { playerIsWizard = True }
          jsonData = encode wizardPlayer
          decodedPlayer = decode jsonData :: Maybe Player
      decodedPlayer `shouldBe` Just wizardPlayer
      case decodedPlayer of
        Just p -> playerIsWizard p `shouldBe` True
        Nothing -> expectationFailure "Failed to decode player"

  describe "Player persistence" $ do
    it "can save and load a player" $ do
      withTempDirectory "player-save-test" $ \tempDir -> do
        let player = createTestPlayer
        
        -- Save the player
        result <- savePlayer tempDir player
        result `shouldBe` Right ()
        
        -- Load the player
        loadResult <- loadPlayer tempDir (playerUsername player)
        loadResult `shouldBe` Right player
        
    it "can save and load a player with wizard status" $ do
      withTempDirectory "player-wizard-save-test" $ \tempDir -> do
        let player = (createTestPlayer) { playerIsWizard = True }
        
        -- Save the player
        result <- savePlayer tempDir player
        result `shouldBe` Right ()
        
        -- Load the player
        loadResult <- loadPlayer tempDir (playerUsername player)
        loadResult `shouldBe` Right player
        
        -- Explicitly check wizard status
        case loadResult of
          Right p -> playerIsWizard p `shouldBe` True
          Left err -> expectationFailure $ "Failed to load player: " ++ err

    it "returns an error when loading a non-existent player" $ do
      withTempDirectory "player-load-test" $ \tempDir -> do
        loadResult <- loadPlayer tempDir "nonexistentPlayer"
        case loadResult of
          Left err -> err `shouldContain` "not found"
          Right _ -> expectationFailure "Expected an error, but got a player"

    it "can handle special characters in player names" $ do
      withTempDirectory "player-special-chars-test" $ \tempDir -> do
        let username = "test-user@with.special_chars" :: Text
            passHash = "hashedPassword123" :: Text
            startingRoom = RoomRef "startRoom"
            playerRef = InstRef "std.player" "123"
            player = mkDefaultPlayer username passHash startingRoom playerRef
        
        -- Save the player
        saveResult <- savePlayer tempDir player
        saveResult `shouldBe` Right ()
        
        -- Load the player
        loadResult <- loadPlayer tempDir username
        loadResult `shouldBe` Right player