{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}

module Game.Scripts.LuaBindings
  ( getShortAction,
    notifyEnteredInvAction
  ) where

--import HsLua.Core
--import HsLua.Core.Error()
import HsLua
import qualified Data.ByteString.Char8 as BS8
import Game.Scripts.LuaCtx
import Game.Types.Object (SomeObjectRef(SomeRef), ObjectRef(..), ObjectKind(..))

-- | Uses Lua's createtable to push a table onto the
-- stack with two fields: ref and name that correspond to
-- the object ref's name. Use the text "placeholder" for
-- the ref's name field.
pushObjectRef :: SomeObjectRef -> LuaM ()
pushObjectRef (SomeRef ref) = do
    -- Create a new table with 0 array elements and 2 hash elements
    createtable 0 2
    
    -- Set the 'ref' field to the string representation of the object reference
    pushstring (BS8.pack $ show ref)
    setfield (-2) "ref"
    
    -- Set the 'name' field to "placeholder" as specified in the comment
    pushstring "placeholder"
    setfield (-2) "name"

-- | Calls the on_entered_inv Lua function, using pushObjectRef to
-- add the object references as tables
notifyEnteredInvAction :: SomeObjectRef -> ObjectRef 'RoomK -> LuaM (Either String ())
notifyEnteredInvAction object from  = do
    -- Get the global Lua function named "on_entered_inv"
    _<- getglobal "on_entered_inv"

    -- Check if it's a function
    isFunc <- isfunction (-1)
    if not isFunc
      then do
        pop 1
        return $ Left "on_entered_inv is not defined or not a function"
      else do
        -- Push the object references as tables
        pushObjectRef object-- The object that entered the inventory
        pushObjectRef (SomeRef from)-- The object from which it came
        
        -- Call the function with 2 arguments and 0 results
        callStatus <- pcall 2 0 Nothing
        if callStatus /= OK
          then do
            errorMsg <- tostring (-1)
            pop 1
            return $ Left $
              case errorMsg of
                Just bs -> "Error calling on_entered_inv: " ++ BS8.unpack bs
                Nothing -> "Error calling on_entered_inv: unknown error"
          else do
            return $ Right ()

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
