-- Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Socket (withSocketsDo)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

import Control.Concurrent.STM (TVar, newTVarIO)
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)

import Config (ServerConfig(..), defaultConfig)
import Network (resolve, open, acceptLoop)
import Game.Actions.Input (clientHandler)
import Game.Types.Room (preloadRooms)
import Game.Scripts.Lua (loadPrototypeList)
import Game.Types.Object(SomeObjectRef(..), ObjectRef(..), ObjectsMap)
import Game.Types.CommandState (CommandState(..))
import Game.Types.Player (Player, PlayerName(..), mkDefaultPlayer)
import Game.Scripts.ScriptMap (emptyScriptMap)
import Network.Socket (Socket)

-- | Create a dummy CommandState for script loading
createDummyCommandState :: TVar (Map.Map PlayerName (Socket, Player)) -> TVar ObjectsMap -> IO CommandState
createDummyCommandState playerMap objectsMap = do
  -- Create a new empty TVar for the script map
  tempScriptMap <- newTVarIO emptyScriptMap
  
  let dummySocket = undefined :: Socket
      dummyName = PlayerName "system"
      dummyRoom = RoomRef "std.room.thevoid"
      dummyPlayerRef = InstRef "std.player" "system"
      dummyPlayer = mkDefaultPlayer "system" "" dummyRoom dummyPlayerRef
  
  return $ CommandState dummySocket dummyName dummyPlayer playerMap objectsMap tempScriptMap

-- | Main entry point
main :: IO ()
main = withSocketsDo $ do
  hSetBuffering stdout NoBuffering
  let config = defaultConfig
  playerMap <- newTVarIO Map.empty
  objectsMap <- newTVarIO preloadRooms
  dummyState <- createDummyCommandState playerMap objectsMap
  scriptMapResult <- loadPrototypeList config dummyState (Map.keys preloadRooms)
  scriptMap <- case scriptMapResult of
    Left err -> do
      putStrLn $ "Error loading scripts: " ++ err
      exitFailure
    Right sm -> newTVarIO sm
  

  addr <- resolve (serverPort config)
  sock <- open addr (serverBacklog config)
  putStrLn $ "Server running on port " ++ serverPort config
  acceptLoop sock playerMap objectsMap scriptMap config clientHandler
