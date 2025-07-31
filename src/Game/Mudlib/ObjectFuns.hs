{-# LANGUAGE OverloadedStrings #-}
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
import qualified Data.ByteString.Char8 as BS8
import Game.Types.Object (SomeObjectRef)

-- | Get the short description of an object by calling the "short" function in Lua.
-- Returns Either an error message or the short description string.
getShort :: Lua.State -> IO (Either String String)
getShort luaState = do
  result <- try (runWith luaState getShortAction)

  case result of
    Left (e :: SomeException) -> return $ Left $ "Exception: " ++ show e
    Right r                   -> return r

notifyEnteredInv :: SomeObjectRef -> SomeObjectRef-> Lua.State -> IO (Either String ())
notifyEnteredInv mover target luaState = do
    result <- try (runWith luaState $ notifyEnteredInvAction mover target)

    case result of
        Left (e :: SomeException) -> return $ Left $ "Exception:" ++ show e
        Right r                   -> return r