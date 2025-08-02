{-# LANGUAGE OverloadedStrings #-}

module Game.Actions.MortalCommands
  ( handleMortalCommand
  , Command
  , CommandHandler
  , commandRegistry
  ) where

import Game.Actions.Commands (Command(..), CommandHandler, CommandRegistry, handleCommand, createHelpHandler, registerCommand)
import Game.Monad (GameM, writeLine, getCurrentEnvironment, displayRoomDescription)
import Game.Types.Object (SomeObjectRef(..), ObjectRef(..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Game.Types.Room


-- | Registry of all available mortal commands
commandRegistry :: CommandRegistry
commandRegistry = 
  let emptyRegistry = Map.empty
      lookCmd = Command { cmdHandler = cmdExamine, cmdHelp = "Examine (look at) some object or the current environment", cmdPrimary = "" }
      helpCmd = Command { cmdHandler = cmdHelpHandler, cmdHelp = "Display help for available commands", cmdPrimary = "" }
  in registerCommand ["look", "examine", "ex", "l"] lookCmd $
     registerCommand ["help"] helpCmd emptyRegistry

-- | Handle mortal commands (no prefix)
handleMortalCommand :: T.Text -> GameM Bool
handleMortalCommand = handleCommand commandRegistry ""

-- | Examine command handler
cmdExamine :: CommandHandler
cmdExamine args = do
  -- Get the current environment reference
  envRef <- getCurrentEnvironment
  
  -- Display the room description
  displayRoomDescription envRef
  
  return True

-- | Help command handler
cmdHelpHandler :: CommandHandler
cmdHelpHandler = createHelpHandler commandRegistry "" "Available commands:"
