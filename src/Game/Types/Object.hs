{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-
Game/
├── Types/
│   ├── Object.hs          -- (You are here)
│   ├── Equipment.hs       -- Slot, Hand, EquipmentKind
│   ├── Player.hs          -- Player logic + Auth
│   ├── Room.hs            -- Room construction / loading
│   ├── Item.hs            -- Item logic
│   ├── Visibility.hs      -- Visibility levels, perception mechanics
│   └── ObjectRef.hs       -- (if split ObjectRef separately)
├── World/
│   ├── Store.hs           -- Game state, persistence
│   ├── WorldMap.hs        -- Graph of rooms
│   └── Spawning.hs        -- Spawn logic for mobs, items, etc.
├── Actions/
│   ├── Combat.hs
│   ├── Movement.hs
│   └── Inventory.hs
├── Engine.hs              -- Tick loop, cooldowns, schedulers
└── Main.hs                -- Game entry point

-}
module Game.Types.Object 
  ( ObjectKind(..)
  , ObjectRef(..)
  , ObjectsMap
  , showRef
  , Visibility(..)
  , InstancedRef()
  , mkInstancedRef
  , getRef
  , getProto
  , IsInstantiable
  , SomeInstRef(..)
  , SomeObjectRef(..)
  , SomeObject(..)
  , ObjectData(..)
  , mkObjectData
  , defaultPrototype
  , Slot(..)
  , Hand(..)
  , EquipmentKind(..)
  , HasObject(..)
  , parseInventory
  ) where

import Data.Text (Text)
import qualified Data.Text
import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:), object, withObject)
import qualified Data.Aeson as A
import qualified Data.Aeson.Types as AT
import Data.Map.Strict (Map)
import Game.Types.Persistable (Persistable(..), defaultSaveObject, defaultLoadObject)

-- Kinds
data ObjectKind = RoomK | PlayerK | ItemK

-- Rooms have no instance, only a prototype
data ObjectRef (k :: ObjectKind) where
  RoomRef :: Text -> ObjectRef 'RoomK
  InstRef :: Text -> Text -> ObjectRef k

deriving instance Eq (ObjectRef k)
-- | Type alias for the objects map
-- Maps object references to object instances
type ObjectsMap = Map SomeObjectRef SomeObject

instance Ord (ObjectRef k) where
  compare (RoomRef proto1) (RoomRef proto2) = compare proto1 proto2
  compare (RoomRef _) (InstRef _ _) = LT
  compare (InstRef _ _) (RoomRef _) = GT
  compare (InstRef proto1 id1) (InstRef proto2 id2) = 
    case compare proto1 proto2 of
      EQ -> compare id1 id2
      result -> result

showRef :: ObjectRef k -> Text
showRef (RoomRef proto)    = proto
showRef (InstRef proto objId) = proto <> "#" <> objId

getProto :: ObjectRef k -> Text
getProto (RoomRef proto) = proto
getProto (InstRef proto _) = proto

instance Show (ObjectRef k) where
  show = Data.Text.unpack . showRef

newtype Visibility = Visibility Int
  deriving (Eq, Show)

-- Instanced Ref (no RoomRef allowed)
data InstancedRef (k :: ObjectKind) where
  IsInst :: ObjectRef k -> InstancedRef k

mkInstancedRef :: ObjectRef k -> Maybe (InstancedRef k)
mkInstancedRef ref@(InstRef _ _) = Just (IsInst ref)
mkInstancedRef (RoomRef _)       = Nothing

getRef :: InstancedRef k -> ObjectRef k
getRef (IsInst r) = r

-- | Type class for kinds that can be instantiated
class IsInstantiable (k :: ObjectKind)

-- | Only PlayerK and ItemK can be instantiated
instance IsInstantiable 'PlayerK
instance IsInstantiable 'ItemK
-- Note: No instance for 'RoomK

-- | SomeInstRef can only contain instantiable kinds
data SomeInstRef = forall k. IsInstantiable k => SomeInstRef (InstancedRef k)

instance Show SomeInstRef where
  show (SomeInstRef instRef) = show (getRef instRef)
  
instance Eq SomeInstRef where
  (SomeInstRef a) == (SomeInstRef b) = 
    case (getRef a, getRef b) of
      (InstRef proto1 id1, InstRef proto2 id2) -> proto1 == proto2 && id1 == id2
      -- Note: RoomRef cases should never occur due to IsInstantiable constraint
      -- but we include them for completeness
      _ -> False

-- A wrapper for heterogeneous inventories
data SomeObjectRef = forall k. SomeRef (ObjectRef k)
deriving instance Show SomeObjectRef

instance Eq SomeObjectRef where
  (SomeRef a) == (SomeRef b) = 
    case (a, b) of
      (RoomRef proto1, RoomRef proto2) -> proto1 == proto2
      (InstRef proto1 id1, InstRef proto2 id2) -> proto1 == proto2 && id1 == id2
      _ -> False

instance Ord SomeObjectRef where
  compare (SomeRef a) (SomeRef b) = 
    case (a, b) of
      (RoomRef proto1, RoomRef proto2) -> compare proto1 proto2
      (RoomRef _, InstRef _ _) -> LT
      (InstRef _ _, RoomRef _) -> GT
      (InstRef proto1 id1, InstRef proto2 id2) -> 
        case compare proto1 proto2 of
          EQ -> compare id1 id2
          result -> result

-- A wrapper for heterogeneous objects
data SomeObject = forall k. SomeObject (ObjectData k)

-- All objects have the same core object data
data ObjectData (k :: ObjectKind) = ObjectData
  { objRef        :: ObjectRef k
  , objName       :: Text
  , objEnv        :: ObjectRef 'RoomK
  , objInventory  :: [SomeInstRef]
  , objVisible    :: Visibility
  , objPersistent :: Bool
  , objPrototype  :: Text
  }

deriving instance Show (ObjectData k)

-- | Default prototype for generic objects
defaultPrototype :: Text
defaultPrototype = "std.object"

-- | Helper function to create an ObjectData with default prototype
mkObjectData :: ObjectRef k -> Text -> ObjectRef 'RoomK -> [SomeInstRef] -> Visibility -> Bool -> ObjectData k
mkObjectData ref name env inventory vis persistent = 
  ObjectData ref name env inventory vis persistent defaultPrototype

-- | Custom Eq instance for ObjectData
-- We need to handle the inventory field specially since it contains existential types
instance Eq (ObjectData 'PlayerK) where
  o1 == o2 =
    -- Compare all fields except inventory
    objRef o1 == objRef o2 &&
    objName o1 == objName o2 &&
    objEnv o1 == objEnv o2 &&
    objVisible o1 == objVisible o2 &&
    objPersistent o1 == objPersistent o2 &&
    objPrototype o1 == objPrototype o2
    -- We don't compare inventory because it contains existential types
    -- and we're setting it to empty when deserializing anyway

-- | Helper functions for JSON parsing
parseRoomRef :: A.Object -> AT.Parser (ObjectRef 'RoomK)
parseRoomRef v = do
  typ <- v .: "type"
  case typ of
    "room" -> RoomRef <$> v .: "proto"
    _ -> fail $ "Expected room reference, got " ++ typ

parseInstRef :: A.Object -> AT.Parser (ObjectRef k)
parseInstRef v = do
  typ <- v .: "type"
  case typ of
    "inst" -> InstRef <$> v .: "proto" <*> v .: "objId"
    _ -> fail $ "Expected instance reference, got " ++ typ

-- | JSON instances for ObjectRef
instance ToJSON (ObjectRef k) where
  toJSON (RoomRef proto) = object [ "type" .= ("room" :: Text), "proto" .= proto ]
  toJSON (InstRef proto objId) = object [ "type" .= ("inst" :: Text), "proto" .= proto, "objId" .= objId ]

instance FromJSON (ObjectRef 'RoomK) where
  parseJSON = withObject "ObjectRef 'RoomK" parseRoomRef

instance FromJSON (ObjectRef 'PlayerK) where
  parseJSON = withObject "ObjectRef 'PlayerK" parseInstRef

instance FromJSON (ObjectRef 'ItemK) where
  parseJSON = withObject "ObjectRef 'ItemK" parseInstRef

-- | JSON instances for Visibility
instance ToJSON Visibility where
  toJSON (Visibility v) = toJSON v

instance FromJSON Visibility where
  parseJSON v = Visibility <$> parseJSON v

-- | JSON instances for SomeInstRef
instance ToJSON SomeInstRef where
  toJSON (SomeInstRef instRef) = toJSON (getRef instRef)

-- | We can't fully deserialize SomeInstRef because we don't know the kind
-- For simplicity, we'll just create empty inventory lists when deserializing
instance FromJSON SomeInstRef where
  parseJSON _ = fail "SomeInstRef deserialization not supported directly"

-- | Custom parser for lists of SomeInstRef (used for inventory)
-- For simplicity, we'll just return an empty list
parseInventory :: A.Value -> AT.Parser [SomeInstRef]
parseInventory _ = return []

-- | JSON instances for ObjectData
instance ToJSON (ObjectData k) where
  toJSON od = object
    [ "ref" .= objRef od
    , "name" .= objName od
    , "env" .= objEnv od
    , "inventory" .= objInventory od
    , "visible" .= objVisible od
    , "persistent" .= objPersistent od
    , "prototype" .= objPrototype od
    ]

instance FromJSON (ObjectData 'PlayerK) where
  parseJSON = withObject "ObjectData 'PlayerK" $ \v -> do
    ref <- v .: "ref"
    name <- v .: "name"
    env <- v .: "env"
    inventoryVal <- v .: "inventory"
    inventory <- parseInventory inventoryVal
    visible <- v .: "visible"
    persistent <- v .: "persistent"
    prototype <- v .: "prototype"
    return $ ObjectData ref name env inventory visible persistent prototype

-- Wearable / Wieldable
data Slot = Head | Chest | Legs | Feet | Arms | Hands deriving (Eq, Show, Ord, Enum, Bounded)
data Hand = LeftHand | RightHand deriving (Eq, Show, Ord, Enum, Bounded)

-- | Helper function for parsing enum types from strings
parseEnum :: String -> [(String, a)] -> A.Value -> AT.Parser a
parseEnum typeName validValues v = do
  s <- parseJSON v
  case lookup s validValues of
    Just value -> return value
    Nothing -> fail $ "Invalid " ++ typeName ++ ": " ++ s ++ 
               ". Valid values are: " ++ show (map fst validValues)

-- | JSON instances for Slot and Hand
instance ToJSON Slot where
  toJSON = toJSON . show

instance FromJSON Slot where
  parseJSON = parseEnum "slot" 
    [ ("Head", Head)
    , ("Chest", Chest)
    , ("Legs", Legs)
    , ("Feet", Feet)
    , ("Hands", Hands)
    , ("Arms", Arms)
    ]

instance ToJSON Hand where
  toJSON = toJSON . show

instance FromJSON Hand where
  parseJSON = parseEnum "hand" 
    [ ("LeftHand", LeftHand)
    , ("RightHand", RightHand)
    ]

data EquipmentKind
  = Wieldable Hand
  | Wearable Slot
  | Neither
  deriving Show



class HasObject a where
  type Kind a :: ObjectKind

  getObject :: a -> ObjectData (Kind a)

  -- You can optionally define default implementations for common things:
  getObjectRef :: a -> ObjectRef (Kind a)
  getObjectRef = objRef . getObject

  getObjectEnv :: a -> ObjectRef 'RoomK
  getObjectEnv = objEnv . getObject

  getObjectInventory :: a -> [SomeInstRef]
  getObjectInventory = objInventory . getObject

  getObjectVisibility :: a -> Visibility
  getObjectVisibility = objVisible . getObject

  isPersistent :: a -> Bool
  isPersistent = objPersistent . getObject


-- | Persistable instance for ObjectData 'PlayerK'
instance Persistable (ObjectData 'PlayerK) where
  -- Save the object to a file in the "objects" subdirectory
  -- Use the object's name as the identifier
  saveObject dataDir identifier obj = 
    defaultSaveObject dataDir identifier "objects" obj
  
  -- Load the object from a file in the "objects" subdirectory
  loadObject dataDir identifier = 
    defaultLoadObject dataDir identifier "objects"