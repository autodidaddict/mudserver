-- Commands.hs
{-# LANGUAGE OverloadedStrings #-}

module Game.Actions.Commands
  ( Command(..)
  , CommandHandler
  , CommandRegistry
  , handleCommand
  , createHelpHandler
  , registerCommand
  ) where

import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Game.Monad (GameM, writeLine)
import Data.Default (Default(def))

-- | Type for command handlers
type CommandHandler = [T.Text] -> GameM Bool

-- | Type for a command with its handler and help text
data Command = Command
  { cmdHandler :: CommandHandler  -- ^ Function that handles the command
  , cmdHelp    :: T.Text          -- ^ Help text for the command
  , cmdPrimary :: T.Text          -- ^ Primary command name (first in the list)
  }

-- | Constructor with default empty primary command
-- This allows backward compatibility with existing code
instance Default Command where
  def = Command { cmdHandler = \_ -> return True, cmdHelp = "", cmdPrimary = "" }

-- | Type alias for a registry of commands
type CommandRegistry = Map.Map T.Text Command

-- | Register a command with its aliases
-- The first name in the list is considered the primary command name
-- and will be shown in help listings
registerCommand :: [T.Text] -> Command -> CommandRegistry -> CommandRegistry
registerCommand [] _ registry = registry
registerCommand (name:aliases) cmd registry = 
  let cmdWithPrimary = cmd { cmdPrimary = name }
      updatedRegistry = Map.insert name cmdWithPrimary registry
  in foldr (\alias reg -> Map.insert alias cmdWithPrimary reg) updatedRegistry aliases

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
-- Only shows primary command names (not aliases) in the general help listing
createHelpHandler :: CommandRegistry -> T.Text -> T.Text -> CommandHandler
createHelpHandler registry prefix helpMessage args = do
  case args of
    [] -> do
      writeLine helpMessage
      -- Group commands by their help text and handler to identify unique commands
      let cmdList = Map.toList registry
          -- Group by help text and handler to identify unique commands
          groupedByHelp = Map.fromListWith (++) [(cmdHelp cmd, [(name, cmd)]) | (name, cmd) <- cmdList]
          -- Get unique commands (one per help text)
          uniqueCmds = Map.elems groupedByHelp
          -- For each group, find the command with the primary name
          primaryCmds = [findPrimaryCmd namesCmds | namesCmds <- uniqueCmds]
      
      -- Display only the primary commands
      mapM_ (\cmd -> writeLine $ "  " <> prefix <> cmdPrimary cmd <> " - " <> cmdHelp cmd) primaryCmds
    
    (cmdName:_) -> do
      case Map.lookup cmdName registry of
        Just cmd -> writeLine $ "Help for " <> prefix <> cmdName <> ": " <> cmdHelp cmd
        Nothing -> writeLine $ "No help available for unknown command: " <> cmdName
  
  return True
  where
    -- Find the command with the primary name in a list of (name, cmd) pairs
    findPrimaryCmd :: [(T.Text, Command)] -> Command
    findPrimaryCmd namesCmds = 
      -- Try to find a command where name matches cmdPrimary
      case filter (\(name, cmd) -> name == cmdPrimary cmd) namesCmds of
        (_, cmd):_ -> cmd  -- Found a match
        _          -> snd $ head namesCmds  -- Fallback to first command