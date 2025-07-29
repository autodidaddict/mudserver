{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Types.Equipment where

import Game.Types.Object (ObjectRef, ObjectKind(..), HasObject(..), Slot(..), Hand(..))
import qualified Data.Map.Strict as M

-- What's currently worn
type Equipped = M.Map Slot (ObjectRef 'ItemK)

-- What's currently held in hands (wielded items)
type Wielded  = M.Map Hand (ObjectRef 'ItemK)

-- What a character *can* wear
type AvailableSlots = [Slot]

-- What hands the player can wield with (in case of limitations)
type AvailableHands = [Hand]

class HasObject a => Wearer a where
  getEquipped       :: a -> Equipped
  getAvailableSlots :: a -> AvailableSlots
  setEquipped       :: a -> Equipped -> a

class HasObject a => Wielder a where
  getWielded       :: a -> Wielded
  getAvailableHands :: a -> AvailableHands
  setWielded       :: a -> Wielded -> a

