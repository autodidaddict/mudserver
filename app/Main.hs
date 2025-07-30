-- Main.hs
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.Socket (withSocketsDo)
import System.IO (hSetBuffering, BufferMode(NoBuffering), stdout)

import Control.Concurrent.STM (newTVarIO)
import qualified Data.Map.Strict as Map
import System.Exit (exitFailure)

import Config (ServerConfig(..), defaultConfig)
import Network (resolve, open, acceptLoop)
import Game.Actions.Input (clientHandler)
import Game.Types.Room (preloadRooms)
import Game.Scripts.Lua (loadPrototypeList)
import Game.Types.Object()


-- | Main entry point
main :: IO ()
main = withSocketsDo $ do
  hSetBuffering stdout NoBuffering
  let config = defaultConfig
  playerMap <- newTVarIO Map.empty
  objectsMap <- newTVarIO preloadRooms
  scriptMapResult <- loadPrototypeList config (Map.keys preloadRooms)
  scriptMap <- case scriptMapResult of
    Left err -> do
      putStrLn $ "Error loading scripts: " ++ err
      exitFailure
    Right sm -> newTVarIO sm
  

  addr <- resolve (serverPort config)
  sock <- open addr (serverBacklog config)
  putStrLn $ "Server running on port " ++ serverPort config
  acceptLoop sock playerMap objectsMap scriptMap config clientHandler
