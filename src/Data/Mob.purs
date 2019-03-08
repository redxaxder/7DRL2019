module Data.Mob
       ( MobType
       , mobs
       , getMobRecord
       )
       where

import Extra.Prelude

import Data.Map (Map)
import Data.Map as Map

import Data.Sprite (Sprite, spriteAt)

newtype MobType = MobType String
derive instance eqMobType :: Eq MobType
derive instance ordMobType :: Ord MobType

type MobRecord =
  { mobType :: MobType
  , name :: String
  , sprite :: Sprite
  , hp :: Int
  --, attributes :: Array Attribute
  }

m :: Int -> Int -> String -> Int -> MobRecord
m = mkMob

mkMob :: Int -> Int -> String -> Int -> MobRecord
mkMob x y name hp =
  { mobType: MobType name
  , name
  , sprite: spriteAt x y
  , hp: hp
  }

mobRecords :: Array MobRecord
mobRecords =
  [ m 0 1 "Bananamatronic Husk" 5
  , m 5 0 "Monion"              5
  , m 6 0 "Tomatosaurus"        5
  , m 7 0 "Meatotaur"           5
  , m 1 1 "Deep Lettuce"        5
  ]

mobs :: Array MobType
mobs = _.mobType <$> mobRecords

mobMap :: Map MobType MobRecord
mobMap = keyBy _.mobType mobRecords

getMobRecord :: MobType -> MobRecord
getMobRecord t = unsafeFromJust $ Map.lookup t mobMap
  -- this is safe as long as MobType is only ever constructed within mobRecords