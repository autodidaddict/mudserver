-- Movement.hs
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.World.Movement
  ( move
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (atomically, modifyTVar')
import qualified Data.Map.Strict as Map
import Game.Monad (GameM)
import Game.Types.Object (SomeObjectRef(..), ObjectRef(..), SomeObject(..), ObjectData(..), ObjectKind(..))
import Game.World.GameObjects (removeObjectFromEnvironment, addObjectToEnvironment, getObject, allObjects)

-- | Move an object from its current environment to a new environment
-- This function:
-- 1. Removes the object from its current environment
-- 2. Updates the object's environment reference to the new environment
-- 3. Adds the object to the new environment
-- Returns True if both the remove and add operations were successful
move :: SomeObjectRef -> ObjectRef 'RoomK -> GameM Bool
move objRef newEnv = do
  -- First, get the object to ensure it exists
  maybeObj <- getObject objRef
  case maybeObj of
    Nothing -> return False
    Just (SomeObject obj) -> do
      -- First remove the object from its current environment
      removeSuccess <- removeObjectFromEnvironment objRef
      
      -- If removal was successful, update the object's environment and add it to the new environment
      if removeSuccess
        then do
          -- Update the object's environment reference in the global objects map
          let updatedObj = obj { objEnv = newEnv }
          objMapTVar <- allObjects
          liftIO $ atomically $ modifyTVar' objMapTVar $ \objMap ->
            Map.insert objRef (SomeObject updatedObj) objMap
          
          -- Add the object to the new environment
          addSuccess <- addObjectToEnvironment objRef
          
          -- Return True only if both operations were successful
          return addSuccess
        else
          return False