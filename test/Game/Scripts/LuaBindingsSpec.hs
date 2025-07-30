{-# LANGUAGE OverloadedStrings #-}

module Game.Scripts.LuaBindingsSpec (spec) where

import Test.Hspec
import HsLua hiding (error)
import qualified Data.ByteString.Char8 as BS8
import System.FilePath ((</>))
import Control.Monad.IO.Class (liftIO)

import Game.Scripts.LuaBindings (notifyEnteredInvAction)
import Game.Types.Object (SomeObjectRef(..), ObjectRef(..))
import Game.Scripts.LuaCtx (LuaM)

-- | Helper function to load a Lua script and run a test with it
withLuaScript :: FilePath -> LuaM a -> IO a
withLuaScript scriptPath action = do
  luaState <- newstate
  runWith luaState $ do
    openlibs
    status <- loadfile (Just scriptPath)
    if status /= OK
      then do
        errorMsg <- tostring (-1)
        error $ "Failed to load script: " ++ maybe "unknown error" BS8.unpack errorMsg
      else do
        status' <- pcall 0 multret Nothing
        if status' /= OK
          then do
            errorMsg <- tostring (-1)
            error $ "Failed to execute script: " ++ maybe "unknown error" BS8.unpack errorMsg
          else action

-- | Check if the entered_inv_called variable is true
checkEnteredInvCalled :: LuaM Bool
checkEnteredInvCalled = do
  _ <- getglobal "was_entered_inv_called"
  isFunc <- isfunction (-1)
  if not isFunc
    then do
      pop 1
      error "was_entered_inv_called is not defined or not a function"
    else do
      callStatus <- pcall 0 1 Nothing
      if callStatus /= OK
        then do
          errorMsg <- tostring (-1)
          error $ "Error calling was_entered_inv_called: " ++ 
            maybe "unknown error" BS8.unpack errorMsg
        else do
          result <- toboolean (-1)
          pop 1
          return result

spec :: Spec
spec = do
  describe "notifyEnteredInvAction" $ do
    it "calls the on_entered_inv Lua function with object references" $ do
      -- Create test object references
      let fromObj = SomeRef (RoomRef "test.room")
          toObj = SomeRef (InstRef "test.player" "player1")
          
      -- Get the path to the test script
      let scriptPath = "test" </> "Game" </> "Scripts" </> "test_on_entered_inv.lua"
      
      -- Run the test with the script
      result <- withLuaScript scriptPath $ do
        -- Call notifyEnteredInvAction with the test objects
        notifyResult <- notifyEnteredInvAction fromObj toObj
        case notifyResult of
          Left err -> error $ "notifyEnteredInvAction failed: " ++ err
          Right _ -> do
            -- Check if the entered_inv_called variable was set to true
            checkEnteredInvCalled
      
      -- Verify that the function was called
      result `shouldBe` True