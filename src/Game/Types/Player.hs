{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Game.Types.Player 
  ( Player(..)
  , mkDefaultPlayer
  , savePlayer
  , loadPlayer
  , PlayerName(..)
  , PlayerMap
  ) where

import Game.Types.Object (ObjectData(..), ObjectRef(..), ObjectKind(..), HasObject(..), Visibility(..), Slot(..), Hand(..))
import Game.Types.Equipment (Equipped, Wielded, AvailableSlots, AvailableHands, Wearer(..), Wielder(..))
import Game.Types.Persistable (Persistable(..), defaultSaveObject, defaultLoadObject)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), object, withObject)
import GHC.Generics (Generic)
import Network.Socket (Socket)

-- | Newtype for player names to improve type safety
newtype PlayerName = PlayerName { unPlayerName :: T.Text }
  deriving (Eq, Ord, Show)

-- | Type alias for the player map
-- Maps player names to (socket, player object) pairs
-- Uses a type parameter p for the player type to avoid circular dependencies
type PlayerMap p = Map PlayerName (Socket, p)

data Player = Player
  { playerBase          :: ObjectData 'PlayerK
  , playerUsername      :: Text
  , playerPasswordHash  :: Text
  , playerXP            :: Int
  , playerEquipped      :: Equipped       -- worn gear
  , playerWielded       :: Wielded        -- held/wielded gear
  , playerWearableSlots :: AvailableSlots
  , playerWieldableHands :: AvailableHands
  , playerIsWizard      :: Bool           -- flag indicating if player has wizard privileges
  } deriving (Show, Generic)

-- | Custom Eq instance for Player
-- We need to handle the inventory field specially since it contains existential types
instance Eq Player where
  p1 == p2 =
    -- Compare all fields except playerBase
    playerUsername p1 == playerUsername p2 &&
    playerPasswordHash p1 == playerPasswordHash p2 &&
    playerXP p1 == playerXP p2 &&
    playerEquipped p1 == playerEquipped p2 &&
    playerWielded p1 == playerWielded p2 &&
    playerWearableSlots p1 == playerWearableSlots p2 &&
    playerWieldableHands p1 == playerWieldableHands p2 &&
    playerIsWizard p1 == playerIsWizard p2 &&
    -- For playerBase, compare all fields except inventory
    objRef (playerBase p1) == objRef (playerBase p2) &&
    objName (playerBase p1) == objName (playerBase p2) &&
    objEnv (playerBase p1) == objEnv (playerBase p2) &&
    objVisible (playerBase p1) == objVisible (playerBase p2) &&
    objPersistent (playerBase p1) == objPersistent (playerBase p2)
    -- We don't compare inventory because it contains existential types
    -- and we're setting it to empty when deserializing anyway

instance HasObject Player where
  type Kind Player = 'PlayerK
  getObject = playerBase

instance Wearer Player where
  getEquipped = playerEquipped
  getAvailableSlots = playerWearableSlots
  setEquipped p eq = p { playerEquipped = eq }

instance Wielder Player where
  getWielded = playerWielded
  getAvailableHands = playerWieldableHands
  setWielded p w = p { playerWielded = w }

-- | JSON instances for Player
instance ToJSON Player where
  toJSON p = object
    [ "base" .= playerBase p
    , "username" .= playerUsername p
    , "passwordHash" .= playerPasswordHash p
    , "xp" .= playerXP p
    , "equipped" .= M.toList (playerEquipped p)
    , "wielded" .= M.toList (playerWielded p)
    , "wearableSlots" .= playerWearableSlots p
    , "wieldableHands" .= playerWieldableHands p
    , "isWizard" .= playerIsWizard p
    ]

instance FromJSON Player where
  parseJSON = withObject "Player" $ \v -> do
    base <- v .: "base"
    username <- v .: "username"
    passwordHash <- v .: "passwordHash"
    xp <- v .: "xp"
    equippedList <- v .: "equipped"
    wieldedList <- v .: "wielded"
    wearableSlots <- v .: "wearableSlots"
    wieldableHands <- v .: "wieldableHands"
    isWizard <- v .: "isWizard"
    return $ Player
      { playerBase = base
      , playerUsername = username
      , playerPasswordHash = passwordHash
      , playerXP = xp
      , playerEquipped = M.fromList equippedList
      , playerWielded = M.fromList wieldedList
      , playerWearableSlots = wearableSlots
      , playerWieldableHands = wieldableHands
      , playerIsWizard = isWizard
      }

-- | Save a player to a file in the dataDirectory + "/players" folder
savePlayer :: String -> Player -> IO (Either String ())
savePlayer dataDir player = defaultSaveObject dataDir (playerUsername player) "players" player

-- | Load a player from a file in the dataDirectory + "/players" folder
loadPlayer :: String -> Text -> IO (Either String Player)
loadPlayer dataDir username = defaultLoadObject dataDir username "players"

mkDefaultPlayer :: Text -> Text -> ObjectRef 'RoomK -> ObjectRef 'PlayerK -> Player
mkDefaultPlayer username passHash startingRoom ref = Player
  { playerBase = ObjectData
      { objRef        = ref
      , objName       = username
      , objEnv        = startingRoom
      , objInventory  = []
      , objVisible    = Visibility 10
      , objPersistent = True
      , objPrototype  = "std.player"  -- Default prototype for players
      }
  , playerUsername       = username
  , playerPasswordHash   = passHash
  , playerXP             = 0
  , playerEquipped       = M.empty
  , playerWielded        = M.empty
  , playerWearableSlots  = [Head, Chest, Legs, Feet, Hands, Arms]
  , playerWieldableHands = [LeftHand, RightHand]
  , playerIsWizard       = False      -- Players are not wizards by default
  }

-- | Persistable instance for Player
instance Persistable Player where
  -- Use the existing savePlayer function
  saveObject dataDir _ player = savePlayer dataDir player
  
  -- Use the existing loadPlayer function
  loadObject = loadPlayer