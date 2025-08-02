{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Actions.Inventory
  ( addToInventory
  , removeFromInventory
  , getInventoryRefs
  ) where

import Game.Types.Object (ObjectData(..), ObjectRef, SomeObjectRef(..), IsInstantiable)

-- | Add an object to another object's inventory
-- The object being added must be instantiable (player or item)
addToInventory :: IsInstantiable j => ObjectData k -> ObjectRef j -> ObjectData k
addToInventory obj instRef =
  obj { objInventory = SomeRef instRef : objInventory obj }

-- | Remove an object from another object's inventory
-- The object being removed must be instantiable (player or item)
removeFromInventory :: IsInstantiable j => ObjectData k -> ObjectRef j -> ObjectData k
removeFromInventory obj instRef =
  obj { objInventory = filter (/= SomeRef instRef) (objInventory obj) }

-- | Get inventory references as a list of SomeObjectRef
-- This is now a simple identity function since inventory is already stored as [SomeObjectRef]
getInventoryRefs :: ObjectData k -> [SomeObjectRef]
getInventoryRefs = objInventory