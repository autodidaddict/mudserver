-- GameMonad.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Types.GameMonad
  ( GameM(..)
  , runGameM
  , rawWrite
  , writeLine
  , getCommandState
  , getScriptMap
  ) where

import Control.Monad.State
import Control.Concurrent.STM (TVar)
import qualified Network.Socket.ByteString as NSB
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Game.Types.CommandState (CommandState(..))
import Game.Scripts.ScriptMap (ScriptMap)

-- | The GameM monad for handling commands with state
newtype GameM a = GameM { unGameM :: StateT CommandState IO a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadState CommandState)

-- | Run a GameM with the given state
runGameM :: CommandState -> GameM a -> IO a
runGameM st action = evalStateT (unGameM action) st

-- | Send raw text to the client socket
rawWrite :: T.Text -> GameM ()
rawWrite msg = do
  sock <- gets clientSocket
  liftIO $ NSB.sendAll sock (TE.encodeUtf8 msg)

-- | Send a line of text to the client (with CRLF)
writeLine :: T.Text -> GameM ()
writeLine s = rawWrite (s `T.append` "\r\n")

-- | Get the current command state
getCommandState :: GameM CommandState
getCommandState = get

-- | Get the script map TVar from the command state
getScriptMap :: GameM (TVar ScriptMap)
getScriptMap = gets (\s -> scriptMap s)