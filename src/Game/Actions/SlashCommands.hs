-- SlashCommands.hs
{-# LANGUAGE OverloadedStrings #-}

module Game.Actions.SlashCommands 
  ( handleSlashCommand
  , Command
  , CommandHandler
  , commandRegistry
  ) where

import Game.Actions.Commands (Command(..), CommandHandler, CommandRegistry, handleCommand, createHelpHandler, registerCommand)
import Game.Monad (GameM, writeLine, playerList, playerName, handleCommandResult)
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
commandRegistry = 
  let emptyRegistry = Map.empty
      quitCmd = Command { cmdHandler = cmdQuit, cmdHelp = "Disconnect from the server", cmdPrimary = "" }
      whoCmd = Command { cmdHandler = cmdWho, cmdHelp = "List all connected players", cmdPrimary = "" }
      tellCmd = Command { cmdHandler = cmdTell, cmdHelp = "Send a private message to another player", cmdPrimary = "" }
      helpCmd = Command { cmdHandler = cmdHelpHandler, cmdHelp = "Display help for available commands", cmdPrimary = "" }
  in registerCommand ["quit", "exit", "logout"] quitCmd $
     registerCommand ["who", "players"] whoCmd $
     registerCommand ["tell", "whisper", "w"] tellCmd $
     registerCommand ["help"] helpCmd emptyRegistry

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
    [] -> 
      handleCommandResult False "Usage: /tell <player> <message>" ""
    [_] -> 
      handleCommandResult False "Please include a message to send" ""
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
          handleCommandResult True ("You sent your message to " <> targetName <> ".") ""
        Nothing -> 
          handleCommandResult False ("Player " <> targetName <> " is not online.") ""

-- | Help command handler
cmdHelpHandler :: CommandHandler
cmdHelpHandler = createHelpHandler commandRegistry "/" "Available commands:"