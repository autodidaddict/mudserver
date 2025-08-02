{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Types.Room 
  ( Room(..)
  , mkDefaultRoom
  , preloadRooms
  ) where

import Game.Types.Object (ObjectData(..), ObjectRef(..), ObjectKind(..), HasObject(..), Visibility(..), SomeObjectRef(..), SomeObject(..))
import Data.Text (Text)
import GHC.Generics (Generic)
import qualified Data.Map.Strict as Map

-- | Room type with objectBase of type ObjectData 'RoomK
data Room = Room
  { roomBase :: ObjectData 'RoomK
  } deriving (Show, Generic)

-- | Custom Eq instance for Room
instance Eq Room where
  r1 == r2 =
    -- Compare all fields of roomBase except inventory
    objRef (roomBase r1) == objRef (roomBase r2) &&
    objName (roomBase r1) == objName (roomBase r2) &&
    objEnv (roomBase r1) == objEnv (roomBase r2) &&
    objVisible (roomBase r1) == objVisible (roomBase r2) &&
    objPersistent (roomBase r1) == objPersistent (roomBase r2)
    -- We don't compare inventory because it contains existential types

-- | HasObject instance for Room
instance HasObject Room where
  type Kind Room = 'RoomK
  getObject = roomBase

-- | Create a default Room from a prototype and a name
-- The objEnv field is set equal to the objRef field (a reference to the room itself)
mkDefaultRoom :: Text -> Text -> Room
mkDefaultRoom prototype name = 
  let roomRef = RoomRef prototype
  in Room
    { roomBase = ObjectData
      { objRef        = roomRef
      , objName       = name
      , objEnv        = roomRef  -- Room is its own environment
      , objInventory  = []
      , objVisible    = Visibility 10
      , objPersistent = False
      , objPrototype  = prototype  -- Use the provided prototype parameter
      }
    }

-- | Create an ObjectsMap with theVoid room
-- This function creates a new room called theVoid with prototype std.room.thevoid
-- and adds it to the objectsMap
preloadRooms :: Map.Map SomeObjectRef SomeObject
preloadRooms = 
  let theVoid = mkDefaultRoom "std.room.thevoid" "The Void"
      voidRef = objRef (roomBase theVoid)
  in Map.singleton (SomeRef voidRef) (SomeObject (roomBase theVoid))