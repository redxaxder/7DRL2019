module Data.Mob
       ( mobs
       , mobName
       , mobSprite
       , Mob
       , MobName
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
  }  
derive instance newtypeMob :: Newtype Mob _

m :: Int -> Int -> String -> Mob
m = mkMob

mkMob :: Int -> Int -> String -> Mob
mkMob x y name = Mob
  { name: MobName name
  , sprite: spriteAt x y
  }

mobs :: Array Mob
mobs =
  [ m 0 1 "Bananamatronic Husk"
  , m 5 0 "Monion"
  , m 6 0 "Tomatosaurus"
  , m 7 0 "Meatotaur"
  , m 1 1 "Deep Lettuce"
  ]

mobName :: Mob -> MobName
mobName = _.name <<< un Mob

mobSprite :: Mob -> Sprite
mobSprite = _.sprite <<< un Mob
