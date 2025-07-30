{-# LANGUAGE DeriveDataTypeable #-}
module Game.Scripts.LuaCtx 
  ( LuaCtx(..)
  , LuaM
  ) where

import HsLua (LuaError, LuaE)
import Control.Exception (Exception)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (get, put, runStateT)
import Game.Monad (GameM(..))
import Game.Types.CommandState (CommandState)

-- phantom type to satisfy LuaError e
data LuaCtx = LuaCtx deriving Show
instance Exception LuaCtx
instance LuaError LuaCtx

type LuaM = LuaE LuaCtx
