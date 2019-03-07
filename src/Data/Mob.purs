module Data.Mob
       ( mobs
       , mobName
       , mobSprite
       )
       where

import Extra.Prelude

import Graphics.Sprite (spriteAt)
import Types (Mob(..), MobName(..), Sprite)

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
