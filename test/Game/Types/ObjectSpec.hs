{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Types.ObjectSpec (spec) where

import Test.Hspec
import qualified Data.Text

import Game.Types.Object

spec :: Spec
spec = do
  describe "ObjectRef Show instance" $ do
    it "uses showRef for RoomRef" $ do
      let roomRef = RoomRef "testRoom" :: ObjectRef 'RoomK
          expected = Data.Text.unpack (showRef roomRef)
      show roomRef `shouldBe` expected

    it "uses showRef for InstRef" $ do
      let instRef = InstRef "std.player" "123" :: ObjectRef 'PlayerK
          expected = Data.Text.unpack (showRef instRef)
      show instRef `shouldBe` expected

    it "formats RoomRef correctly" $ do
      let roomRef = RoomRef "testRoom" :: ObjectRef 'RoomK
      show roomRef `shouldBe` "testRoom"

    it "formats InstRef correctly" $ do
      let instRef = InstRef "std.player" "123" :: ObjectRef 'PlayerK
      show instRef `shouldBe` "std.player#123"