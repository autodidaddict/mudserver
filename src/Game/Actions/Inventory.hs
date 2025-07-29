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

import Game.Types.Object (ObjectData(..), InstancedRef, SomeInstRef(..), getRef, SomeObjectRef(..), IsInstantiable)

addToInventory :: IsInstantiable j => ObjectData k -> InstancedRef j -> ObjectData k
addToInventory obj inst =
  obj { objInventory = SomeInstRef inst : objInventory obj }

removeFromInventory :: IsInstantiable j => ObjectData k -> InstancedRef j -> ObjectData k
removeFromInventory obj inst =
  obj { objInventory = filter (/= SomeInstRef inst) (objInventory obj) }


getInventoryRefs :: ObjectData k -> [SomeObjectRef]  -- untyped refs
getInventoryRefs = map (\(SomeInstRef r) -> SomeRef (getRef r)) . objInventory