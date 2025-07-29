{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Mudlib.ObjectFuns
  ( getShort
  ) where

import HsLua as Lua hiding (error, try)
-- Import instances from HsLua.Core.Error as suggested by GHC warning
import HsLua.Core.Error()
import Control.Exception (try, SomeException)
import qualified Data.ByteString.Char8 as BS8

-- | Get the short description of an object by calling the "short" function in Lua.
-- Returns Either an error message or the short description string.
getShort :: Lua.State -> IO (Either String String)
getShort luaState = do
  let action :: LuaE Exception (Either String String)
      action = do
        -- Get the global Lua function named "short"
        _ <- getglobal "short"

        -- Check if it's a function
        isFunc <- isfunction (-1)
        if not isFunc
          then do
            pop 1
            return $ Left "short is not defined or not a function"
          else do
            callStatus <- pcall 0 1 Nothing
            if callStatus /= OK
              then do
                errorMsg <- tostring (-1)
                pop 1
                return $ Left $
                  case errorMsg of
                    Just bs -> "Error calling short: " ++ BS8.unpack bs
                    Nothing -> "Error calling short: unknown error"
              else do
                resultStr <- tostring (-1)
                pop 1
                return $
                  case resultStr of
                    Just bs -> Right (BS8.unpack bs)
                    Nothing -> Left "short function did not return a string"

  result <- try (runWith luaState action)

  case result of
    Left (e :: SomeException) -> return $ Left $ "Exception: " ++ show e
    Right r                   -> return r
