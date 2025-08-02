{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Mudlib.ObjectFuns
  ( getShort
  , notifyEnteredInv
  ) where

import HsLua as Lua hiding (error, try)
-- Import instances from HsLua.Core.Error as suggested by GHC warning
import HsLua.Core.Error()
import Control.Exception (try, SomeException)
import Game.Scripts.LuaBindings
import Game.Types.Object (ObjectRef(..), SomeObjectRef(..), ObjectKind(..))

-- | Get the short description of an object by calling the "short" function in Lua.
-- Returns the short description string or "An object" if an error occurs.
getShort :: Lua.State -> IO String
getShort luaState = do
  result <- try (runWith luaState getShortAction) :: IO (Either SomeException (Either String String))

  case result of
      Right (Right s)          -> return s
      _ -> return "An object"

notifyEnteredInv :: SomeObjectRef -> ObjectRef 'RoomK -> Lua.State -> IO ()
notifyEnteredInv objectRef fromRef luaState = do
    _ <- try (runWith luaState $ notifyEnteredInvAction objectRef fromRef) :: IO (Either SomeException (Either String ()))
    return ()