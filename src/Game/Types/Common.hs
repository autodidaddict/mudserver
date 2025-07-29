-- Common.hs
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Types.Common
  ( PlayerName(..)
  , PlayerMap
  , ObjectsMap
  ) where

import Data.Map.Strict (Map)
import qualified Data.Text as T
import Network.Socket (Socket)
import Game.Types.Object (SomeObjectRef, SomeObject)

-- | Newtype for player names to improve type safety
newtype PlayerName = PlayerName { unPlayerName :: T.Text }
  deriving (Eq, Ord, Show)

-- | Type alias for the player map
-- Maps player names to (socket, player object) pairs
-- Uses a type parameter p for the player type to avoid circular dependencies
type PlayerMap p = Map PlayerName (Socket, p)

-- | Type alias for the objects map
-- Maps object references to object instances
type ObjectsMap = Map SomeObjectRef SomeObject