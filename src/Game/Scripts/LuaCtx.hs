{-# LANGUAGE DeriveDataTypeable #-}
module Game.Scripts.LuaCtx where

import HsLua (LuaError, LuaE)
import Control.Exception (Exception)

-- phantom type to satisfy LuaError e
data LuaCtx = LuaCtx deriving Show
instance Exception LuaCtx
instance LuaError LuaCtx

type LuaM = LuaE LuaCtx