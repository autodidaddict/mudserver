{-# LANGUAGE OverloadedStrings #-}

module Game.Actions.MortalCommands
  ( handleMortalCommand
  , Command
  , CommandHandler
  , commandRegistry
  ) where

import Game.Actions.Commands (Command(..), CommandHandler, CommandRegistry, handleCommand, createHelpHandler)
import Game.Monad (GameM, writeLine, getCurrentEnvironment, displayRoomDescription)
import Game.Types.Object (SomeObjectRef(..), ObjectRef(..))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import Game.Types.Room


-- | Registry of all available mortal commands
commandRegistry :: CommandRegistry
commandRegistry = Map.fromList
  [ ("look", Command cmdExamine "Examine (look at) some object or the current environment")
  , ("help", Command cmdHelpHandler "Display help for available commands")
  ]

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
