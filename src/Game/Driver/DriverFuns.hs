{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Game.Driver.DriverFuns where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.State (gets)
import Control.Concurrent.STM (readTVar, atomically)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Network.Socket.ByteString as NSB
import qualified Data.Map.Strict as Map
import HsLua

import Game.Scripts.LuaCtx (LuaM, liftGameM)
import Game.Monad (GameM(..))
import Game.Types.CommandState (CommandState(..))
import Game.Types.Player (PlayerName(..))

-- | Register game functions in the Lua environment
registerGameFunctions :: CommandState -> LuaM ()
registerGameFunctions cmdState = do
  pushDocumentedFunction factorial *> setglobal "factorial"
  pushDocumentedFunction (tellPlayer cmdState) *> setglobal "tellPlayer"

-- | Factorial function.
factorial :: DocumentedFunction e
factorial = defun "factorial"
  ### liftPure (\n -> product [1..n] :: Prelude.Integer)
  --                 get arg      type of arg      name  description
  <#> parameter      peekIntegral "integer"        "n"   "input number"
  =#> functionResult pushIntegral "integer|string"       "factorial of n"
  #? "Computes the factorial of an integer."

-- | Tell player function.
tellPlayer :: LuaError e => CommandState -> DocumentedFunction e
tellPlayer cmdState = defun "tellPlayer"
  ### (\targetName message -> do
        let gameAction = do
              pl <- gets playerList
              from <- gets playerName
              players <- liftIO $ atomically $ readTVar pl
              let target = PlayerName (T.pack targetName)
                  messageText = T.pack message
              
              case Map.lookup target players of
                Just (sock, _) -> do
                  let formattedMsg = unPlayerName from <> " tells you: " <> messageText <> "\r\n"
                  liftIO $ NSB.sendAll sock $ TE.encodeUtf8 formattedMsg
                  return True
                Nothing -> 
                  return False
        result <- liftGameM cmdState gameAction
        return result
      )
  <#> parameter peekString "string" "targetName" "name of the player to send message to"
  <#> parameter peekString "string" "message" "message to send"
  =#> functionResult pushBool "boolean" "true if message was sent, false if player not found"
  #? "Sends a message to a player."