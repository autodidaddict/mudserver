{-# LANGUAGE DeriveDataTypeable #-}
module Game.Scripts.LuaCtx 
  ( LuaCtx(..)
  , LuaM
  , liftGameM
  ) where

import HsLua (LuaError, LuaE)
import Control.Exception (Exception)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.State.Strict (get, put, runStateT, evalStateT)
import Game.Monad (GameM, runGameM)
import Game.Types.CommandState (CommandState)

-- phantom type to satisfy LuaError e
data LuaCtx = LuaCtx deriving Show
instance Exception LuaCtx
instance LuaError LuaCtx

type LuaM = LuaE LuaCtx

-- | Lift a GameM action into the LuaE monad with any error type e
liftGameM :: LuaError e => CommandState -> GameM a -> LuaE e a
liftGameM cmdState gameAction = liftIO $ runGameM cmdState gameAction
