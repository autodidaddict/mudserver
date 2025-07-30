{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Game.Scripts.LuaBindings
  ( getShortAction
  ) where

import HsLua.Core
import HsLua.Core.Error()
import qualified Data.ByteString.Char8 as BS8
import Game.Scripts.LuaCtx

getShortAction :: LuaM (Either String String)
getShortAction = do
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
