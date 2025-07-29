-- ScriptMap.hs
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}

module Game.Scripts.ScriptMap
  ( ScriptMap
  , loadPrototype
  , loadPrototypeList
  , unloadPrototype
  , emptyScriptMap
  , loadScript
  , loadScriptDefault
  , SandboxSetup
  ) where

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import System.FilePath ((</>), (<.>))
import System.Directory (doesFileExist)
import Control.Monad.IO.Class (MonadIO)
import Config (ServerConfig(..))
import Data.List (intercalate)
import HsLua as Lua hiding (error, try)
import Control.Exception (try, SomeException)
import qualified Data.ByteString.Char8 as BS8
import Game.Types.Object (SomeObjectRef(..), ObjectRef(..))

import Control.Monad (forM_, foldM)

-- | Type alias for the script map
-- Maps prototype names to Lua states
type ScriptMap = Map.Map T.Text Lua.State

-- | Create an empty script map
emptyScriptMap :: ScriptMap
emptyScriptMap = Map.empty

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
loadPrototype :: (MonadIO m) => ServerConfig -> T.Text -> ScriptMap -> m (Either String ScriptMap)
loadPrototype config prototype scriptMap = 
  if Map.member prototype scriptMap
    then return (Right scriptMap)  -- Already loaded, do nothing
    else do
      let dataDir = dataDirectory config
          scriptPath = dataDir </> prototypeToFilePath prototype <.> "lua"
      
      fileExists <- liftIO $ doesFileExist scriptPath
      if not fileExists
        then return (Left $ "Script file does not exist: " ++ scriptPath)  -- Script file doesn't exist, return error
        else do
          -- Load the script using loadScriptDefault
          result <- liftIO $ loadScriptDefault scriptPath
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
loadPrototypeList :: (MonadIO m) => ServerConfig -> [SomeObjectRef] -> m (Either String ScriptMap)
loadPrototypeList config refs = do
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
              loadPrototype config proto scriptMap
        ) (Right initialMap) refs

-- | A sandbox setup runs inside the Lua monad and can alter the global env.
type SandboxSetup = Lua ()

defaultSandbox :: SandboxSetup
defaultSandbox = do
  let dangerous = ["io", "os", "debug", "package", "require", "dofile", "loadfile"]
  forM_ dangerous $ \nm -> pushnil >> setglobal nm
  settop 0


throwLuaError :: Lua.Status -> Lua a
throwLuaError statusCode = do
  -- Error message should be on top of the stack
  msg <- tostring (-1)
  errMsg <- case msg of
    Just bs -> return $ BS8.unpack bs
    Nothing -> return $ "Lua error with status: " ++ show statusCode
  error errMsg

loadScript :: SandboxSetup -> FilePath -> IO (Either SomeException Lua.State)
loadScript sandbox path = do
  st <- newstate
  try $ runWith st $ do
    openlibs
    sandbox
    statusCode <- loadfile (Just path)
    if statusCode /= OK
      then throwLuaError statusCode
      else do
        statusCode' <- pcall 0 multret Nothing
        if statusCode' /= OK
          then throwLuaError statusCode'
          else return st

-- | Convenience wrapper using the default sandbox.
loadScriptDefault :: FilePath -> IO (Either SomeException Lua.State)
loadScriptDefault = loadScript defaultSandbox

