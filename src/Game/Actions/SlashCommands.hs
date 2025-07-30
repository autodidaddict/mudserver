-- SlashCommands.hs
{-# LANGUAGE OverloadedStrings #-}

module Game.Actions.SlashCommands 
  ( handleSlashCommand
  , Command
  , CommandHandler
  , commandRegistry
  ) where

import Game.Actions.Commands (Command(..), CommandHandler, CommandRegistry, handleCommand, createHelpHandler)
import Game.Monad (GameM, writeLine, playerList, playerName)
import qualified Data.Map.Strict as Map
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (readTVar, atomically)
import Control.Monad.State (gets)
import qualified Network.Socket.ByteString as NSB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Game.Types.Player (PlayerName(..))

-- | Registry of all available slash commands
commandRegistry :: CommandRegistry
commandRegistry = Map.fromList
  [ ("quit", Command cmdQuit "Disconnect from the server")
  , ("who", Command cmdWho "List all connected players")
  , ("tell", Command cmdTell "Send a private message to another player")
  , ("help", Command cmdHelpHandler "Display help for available commands")
  ]

-- | Handle slash commands
handleSlashCommand :: T.Text -> GameM Bool
handleSlashCommand = handleCommand commandRegistry "/"

-- | Quit command handler
cmdQuit :: CommandHandler
cmdQuit _ = do
  writeLine "Goodbye!"
  return False

-- | Who command handler
cmdWho :: CommandHandler
cmdWho _ = do
  pl <- gets playerList
  players <- liftIO $ atomically $ readTVar pl
  writeLine "Connected players:"
  mapM_ (writeLine . ("  " <>) . unPlayerName) (Map.keys players)
  return True

-- | Tell command handler
cmdTell :: CommandHandler
cmdTell args = do
  case args of
    [] -> do
      writeLine "Usage: /tell <player> <message>"
      return True
    [_] -> do
      writeLine "Please include a message to send"
      return True
    (targetName:msgWords) -> do
      pl <- gets playerList
      from <- gets playerName
      players <- liftIO $ atomically $ readTVar pl
      let target = PlayerName targetName
          message = T.unwords msgWords
      
      case Map.lookup target players of
        Just (sock, _) -> do
          let formattedMsg = unPlayerName from <> " tells you: " <> message <> "\r\n"
          liftIO $ NSB.sendAll sock $ TE.encodeUtf8 formattedMsg
          writeLine $ "You sent your message to " <> targetName <> "."
        Nothing -> 
          writeLine $ "Player " <> targetName <> " is not online."
      return True

-- | Help command handler
cmdHelpHandler :: CommandHandler
cmdHelpHandler = createHelpHandler commandRegistry "/" "Available commands:"