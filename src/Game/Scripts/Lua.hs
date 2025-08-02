{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Game.Scripts.Lua 
  ( loadPrototype
  , unloadPrototype
  , loadPrototypeList
  , loadScript
  , loadScriptDefault
  , getLuaState
  ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import System.FilePath ((</>), (<.>))
import System.Directory (doesFileExist)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Config (ServerConfig(..))
import Data.List (intercalate)
import HsLua.Core
import qualified HsLua as Lua
import Control.Exception (try, SomeException)
import Game.Scripts.ScriptMap
import Game.Types.Object (SomeObjectRef(..), ObjectRef(..))
import Control.Monad (forM_, foldM)
import Game.Scripts.LuaCtx
import Control.Monad.State (gets)
import Control.Concurrent.STM (readTVarIO, atomically, modifyTVar')
import Game.Driver.DriverFuns (registerGameFunctions)
import Game.Types.GameMonad (GameM, getCommandState, getScriptMap)
import Game.Types.CommandState (CommandState(..))

-- | A sandbox setup runs inside the Lua monad and can alter the global env.
type SandboxSetup = LuaM ()


-- | Convert a prototype name to a file path
-- e.g., "std.room.thevoid" -> "scripts/std/room/thevoid"
prototypeToFilePath :: T.Text -> FilePath
prototypeToFilePath prototype =
  let parts = T.splitOn "." prototype
      dirs = map T.unpack (init parts)
      filename = T.unpack (last parts)
  in "scripts" </> intercalate "/" dirs </> filename

-- | Load a prototype script into the script map
-- If the prototype is already loaded, does nothing
-- Returns Either an error message or the updated ScriptMap
loadPrototype :: (MonadIO m) => CommandState -> T.Text -> ScriptMap -> m (Either String ScriptMap)
loadPrototype cmdState prototype scriptMap =
  if Map.member prototype scriptMap
    then return (Right scriptMap)  -- Already loaded, do nothing
    else do
      let dataDir = dataDirectory (serverConfig cmdState)
          scriptPath = dataDir </> prototypeToFilePath prototype <.> "lua"

      fileExists <- liftIO $ doesFileExist scriptPath
      if not fileExists
        then return (Left $ "Script file does not exist: " ++ scriptPath)  -- Script file doesn't exist, return error
        else do
          -- Load the script using loadScriptDefault
          result <- liftIO $ loadScriptDefault cmdState scriptPath
          case result of
            Left err -> return (Left $ "Failed to load script: " ++ show err)
            Right luaState -> return (Right $ Map.insert prototype luaState scriptMap)

-- | Unload a prototype script from the script map
-- If the prototype is not loaded, does nothing
-- Properly closes the Lua state when unloading
unloadPrototype :: (MonadIO m) => T.Text -> ScriptMap -> m ScriptMap
unloadPrototype prototype scriptMap =
  if Map.member prototype scriptMap
    then do
      -- Get the Lua state and close it before removing from the map
      let maybeState = Map.lookup prototype scriptMap
      case maybeState of
        Just luaState -> liftIO $ Lua.close luaState
        Nothing -> return ()
      return $ Map.delete prototype scriptMap
    else return scriptMap  -- Not loaded, do nothing

-- | Load a list of prototypes into the script map
-- Takes a list of SomeObjectRef and calls loadPrototype for each one
-- Returns Either an error message or the updated ScriptMap
-- Starts with emptyScriptMap
loadPrototypeList :: (MonadIO m) => CommandState -> [SomeObjectRef] -> m (Either String ScriptMap)
loadPrototypeList cmdState refs = do
  -- Extract prototype name from each SomeObjectRef
  let getPrototype (SomeRef (RoomRef proto)) = proto
      getPrototype (SomeRef (InstRef proto _)) = proto

  -- Start with an empty script map
  let initialMap = emptyScriptMap

  -- Process each prototype one by one
  foldM (\mapSoFar objRef -> do
          case mapSoFar of
            Left err -> return (Left err)  -- If we already have an error, just propagate it
            Right scriptMap -> do
              let proto = getPrototype objRef
              loadPrototype cmdState proto scriptMap
        ) (Right initialMap) refs


defaultSandbox :: SandboxSetup
defaultSandbox = do
  let dangerous = ["io", "os", "debug", "package", "require", "dofile", "loadfile"]
  forM_ dangerous $ \nm -> pushnil >> setglobal nm
  settop 0


-- | Run a Lua script file, applying the sandbox and loading the script.
-- Returns Lua state or an exception.
loadScript
  :: SandboxSetup   -- Sandbox initialization Lua action
  -> CommandState   -- Command state for GameM actions
  -> FilePath       -- Lua script path
  -> IO (Either SomeException Lua.State)
loadScript sandbox cmdState path = do
  lstate <- Lua.newstate
  Control.Exception.try $ Lua.runWith lstate $ do
    Lua.openlibs
    sandbox
    registerGameFunctions cmdState
    status1 <- Lua.loadfile (Just path)
    if status1 /= OK
      then throwLuaError status1
      else do
        status2 <- Lua.pcall 0 multret Nothing
        if status2 /= OK
          then throwLuaError status2
          else return lstate

-- | Throw a Lua error by failing with a textual message
throwLuaError :: Status -> LuaM a
throwLuaError code = Lua.failLua ("Lua error: " <> show code)

-- | Convenience wrapper using the default sandbox.
loadScriptDefault :: CommandState -> FilePath -> IO (Either SomeException Lua.State)
loadScriptDefault cmdState = loadScript defaultSandbox cmdState

-- | Get a Lua state for script execution.
-- This gets the Lua state from the loaded script map using the prototype as a key.
-- If the prototype is not found in the script map, loads the prototype script and updates the script map.
getLuaState :: T.Text -> GameM Lua.State
getLuaState prototype = do
  -- Get the script map from the command state
  scriptMapTVar <- getScriptMap
  scriptMapVal <- liftIO $ readTVarIO scriptMapTVar
  
  -- Look up the prototype in the script map
  case Map.lookup prototype scriptMapVal of
    Just luaState -> return luaState
    Nothing -> do
      -- If not found, load the prototype script using loadPrototype
      cmdState <- getCommandState
      result <- loadPrototype cmdState prototype scriptMapVal
      
      case result of
        Right updatedScriptMap -> do
          -- Update the script map with the new Lua state
          liftIO $ atomically $ modifyTVar' scriptMapTVar (const updatedScriptMap)
          -- Get the Lua state from the updated map
          case Map.lookup prototype updatedScriptMap of
            Just luaState -> return luaState
            -- This should not happen as loadPrototype should have added the state
            Nothing -> createFallbackLuaState
        Left _ -> do
          -- If loading fails, create a new Lua state as fallback
          createFallbackLuaState

-- | Create a fallback Lua state with standard libraries
createFallbackLuaState :: GameM Lua.State
createFallbackLuaState = do
  luaState <- liftIO Lua.newstate
  -- Open standard libraries
  liftIO $ Lua.runWith luaState Lua.openlibs
  return luaState

