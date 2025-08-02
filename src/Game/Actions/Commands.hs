-- Commands.hs
{-# LANGUAGE OverloadedStrings #-}

module Game.Actions.Commands
  ( Command(..)
  , CommandHandler
  , CommandRegistry
  , handleCommand
  , createHelpHandler
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Game.Monad (GameM, writeLine)

-- | Type for command handlers
type CommandHandler = [T.Text] -> GameM Bool

-- | Type for a command with its handler and help text
data Command = Command
  { cmdHandler :: CommandHandler  -- ^ Function that handles the command
  , cmdHelp    :: T.Text          -- ^ Help text for the command
  }

-- | Type alias for a registry of commands
type CommandRegistry = Map.Map T.Text Command

-- | Generic command handler that can be used for both slash and wizard commands
handleCommand :: CommandRegistry -> T.Text -> T.Text -> GameM Bool
handleCommand registry prefix input = 
  case T.words input of
    [] -> do
      writeLine $ "Empty " <> prefix <> " command"
      return True
    (cmd:args) -> 
      case Map.lookup cmd registry of
        Just command -> cmdHandler command args
        Nothing -> do
          if T.length prefix == 0 then writeLine $ "Unknown command: " <> cmd
          else writeLine $ "Unknown " <> prefix <> " command: " <> prefix <> cmd
          return True

-- | Create a help command handler for a command registry
createHelpHandler :: CommandRegistry -> T.Text -> T.Text -> CommandHandler
createHelpHandler registry prefix helpMessage args = do
  case args of
    [] -> do
      writeLine helpMessage
      let cmdList = Map.toList registry
      mapM_ (\(name, cmd) -> writeLine $ "  " <> prefix <> name <> " - " <> cmdHelp cmd) cmdList
    (cmdName:_) ->
      case Map.lookup cmdName registry of
        Just cmd -> writeLine $ "Help for " <> prefix <> cmdName <> ": " <> cmdHelp cmd
        Nothing -> writeLine $ "No help available for unknown command: " <> cmdName
  return True