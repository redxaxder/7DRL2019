module Data.Mob
       ( MobType
       , mobs
       , getMobRecord
       )
       where

import Extra.Prelude

import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Map (Map)
import Data.Map as Map
import Partial.Unsafe (unsafePartial)

import Data.Sprite (Sprite, spriteAt)
import Data.Item (ItemType, stringToItemType)

newtype MobType = MobType String
derive instance eqMobType :: Eq MobType
derive instance ordMobType :: Ord MobType

type MobRecord =
  { mobType :: MobType
  , name :: String
  , sprite :: Sprite
  , hp :: Int
  , drop :: Maybe ItemType
  --, attributes :: Array Attribute
  }

m :: Int -> Int -> String -> Int -> MobRecord
m = mkMob Nothing

n :: Int -> Int -> String -> Int -> String -> MobRecord
n x y name hp drop = mkMob (Just drop) x y name hp

mkMob :: (Maybe String) -> Int -> Int -> String -> Int -> MobRecord
mkMob drop x y name hp = unsafePartial
  { mobType: MobType name
  , name
  , sprite: spriteAt x y
  , hp: hp
  , drop: stringToItemType <$> drop
  }

mobRecords :: Array MobRecord
mobRecords =
  [ n 5 0 "Monion"              5 "whole onion"
  , n 6 0 "Tomatosaurus"        5 "whole tomato"
  , n 7 0 "Meatotaur"           5 "raw meat"
  , n 1 1 "Deep Lettuce"        5 "deep lettuce"
  --, m 0 1 "Bananamatronic Husk" 5
  ]

mobs :: NonEmptyArray MobType
mobs = unsafeFromJust $ fromArray $ _.mobType <$> mobRecords

mobMap :: Map MobType MobRecord
mobMap = keyBy _.mobType mobRecords

getMobRecord :: MobType -> MobRecord
getMobRecord t = unsafeFromJust $ Map.lookup t mobMap
  -- this is safe as long as MobType is only ever constructed within mobRecords
