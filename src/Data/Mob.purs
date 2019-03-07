module Data.Mob
       ( mobs
       , mobName
       , mobSprite
       , Mob(..)
       , MobName(..)
       )
       where

import Extra.Prelude

import Data.Sprite (Sprite, spriteAt)

newtype MobName = MobName String
derive instance eqMobName :: Eq MobName
derive instance ordMobName :: Ord MobName
derive instance newtypeMobName :: Newtype MobName _
newtype Mob = Mob
  { name :: MobName
  , sprite :: Sprite
  , hp :: Int
  }
derive instance newtypeMob :: Newtype Mob _

m :: Int -> Int -> String -> Int -> Mob
m = mkMob

mkMob :: Int -> Int -> String -> Int -> Mob
mkMob x y name hp = Mob
  { name: MobName name
  , sprite: spriteAt x y
  , hp: hp
  }

mobs :: Array Mob
mobs =
  [ m 0 1 "Bananamatronic Husk" 5
  , m 5 0 "Monion"              5
  , m 6 0 "Tomatosaurus"        5
  , m 7 0 "Meatotaur"           5
  , m 1 1 "Deep Lettuce"        5
  ]

mobName :: Mob -> MobName
mobName = _.name <<< un Mob

mobSprite :: Mob -> Sprite
mobSprite = _.sprite <<< un Mob
