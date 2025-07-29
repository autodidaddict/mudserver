{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}

module Game.Types.Item 
  ( Item(..)
  , createItem
  ) where

import Game.Types.Object (EquipmentKind(..), ObjectData(..), ObjectRef(..), ObjectKind(..), Visibility(..))
import Data.Text (Text)
import qualified Data.Text as T
import System.Random (randomRIO)
import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Game.Monad (GameM)
import Game.World.GameObjects (addObject)

data Item = Item
  { itemBase  :: ObjectData 'ItemK
  , itemEquip :: EquipmentKind
  } deriving Show

-- | Generate a random 6-character alphanumeric hash
generateRandomHash :: IO Text
generateRandomHash = do
  let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
      len = length chars
  indices <- replicateM 6 (randomRIO (0, len - 1))
  return $ T.pack [chars !! i | i <- indices]

-- | Create a new item with a random instance ID
-- The item's environment is set to "std.room.thevoid"
-- This function adds the item to the global object map
createItem :: Text -> Text -> GameM Item
createItem prototype name = do
  -- Generate a random 6-character hash for the instance ID
  hash <- liftIO generateRandomHash
  
  -- Create the item reference
  let itemRef = InstRef prototype hash
  
  -- Create the environment reference (the void)
  let voidRef = RoomRef "std.room.thevoid"
  
  -- Create the object data
  let objData = ObjectData
        { objRef = itemRef
        , objName = name
        , objEnv = voidRef
        , objInventory = []
        , objVisible = Visibility 100  -- Fully visible
        , objPersistent = True
        , objPrototype = prototype
        }
  
  -- Add the item to the global object map
  addObject itemRef objData
  
  -- Create and return the item
  return $ Item
    { itemBase = objData
    , itemEquip = Neither
    }

