{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Types.PlayerSpec (spec) where

import Test.Hspec
import Data.Text (Text)
import qualified Data.Map.Strict as M

import Game.Types.Player
import Game.Types.Object

spec :: Spec
spec = do
  describe "mkDefaultPlayer" $ do
    let username = "testUser" :: Text
        passHash = "hashedPassword123" :: Text
        startingRoom = RoomRef "startRoom"
        playerRef = InstRef "std.player" "123"
        player = mkDefaultPlayer username passHash startingRoom playerRef

    it "sets the username correctly" $ do
      playerUsername player `shouldBe` username

    it "sets the password hash correctly" $ do
      playerPasswordHash player `shouldBe` passHash

    it "sets the starting XP to 0" $ do
      playerXP player `shouldBe` 0

    it "sets the equipped items to empty" $ do
      playerEquipped player `shouldBe` M.empty

    it "sets the wielded items to empty" $ do
      playerWielded player `shouldBe` M.empty

    it "sets the correct wearable slots" $ do
      playerWearableSlots player `shouldBe` [Head, Chest, Legs, Feet, Hands, Arms]

    it "sets the correct wieldable hands" $ do
      playerWieldableHands player `shouldBe` [LeftHand, RightHand]

    it "sets the object reference correctly" $ do
      objRef (playerBase player) `shouldBe` playerRef

    it "sets the object name to the username with 's character suffix" $ do
      objName (playerBase player) `shouldBe` username
      
    it "sets the object prototype to std.player" $ do
      objPrototype (playerBase player) `shouldBe` "std.player"

    it "sets the object environment to the starting room" $ do
      objEnv (playerBase player) `shouldBe` startingRoom

    it "sets the inventory to empty" $ do
      objInventory (playerBase player) `shouldBe` []

    it "sets the visibility to 10" $ do
      objVisible (playerBase player) `shouldBe` Visibility 10

    it "sets persistence to true" $ do
      objPersistent (playerBase player) `shouldBe` True
      
    it "sets isWizard to false by default" $ do
      playerIsWizard player `shouldBe` False