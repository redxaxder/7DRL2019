module Types.Mob 
  ( module Types.Mob
  , module Data.Mob
  ) where

import Extra.Prelude

import Atlas (Atlas, Position, move)
import Data.Mob (MobType, getMobRecord)
import Data.Sprite (Sprite)
import Data.Tile (Tile)
import Types.Item (Item, mkItem)
import Direction (Direction)

newtype Mob = Mob { mobType :: MobType, hp :: Int, position :: Position } -- TODO: Add relevant mob state here
derive instance newtypeMob :: Newtype Mob _

hit :: Mob -> Maybe Mob
hit m@(Mob {hp}) =
  let hp' = hp - 1
  in if hp' <= 0
  then Nothing
  else Just $ Mob $ (un Mob m) {hp = hp'}

mkMob :: MobType -> Position -> Mob
mkMob t pos = Mob { mobType: t
              , hp: _.hp $ getMobRecord t
              , position: pos
              }

position :: Mob -> Position
position (Mob m) = m.position

moveMob :: Direction -> Atlas Tile -> Mob -> Mob
moveMob d atlas (Mob m) = Mob (m { position = move d atlas m.position })

mobName :: Mob -> String
mobName = _.name <<< getMobRecord <<< mobType

mobSprite :: Mob -> Sprite
mobSprite = _.sprite <<< getMobRecord <<< mobType

mobType :: Mob -> MobType
mobType (Mob {mobType: t}) = t

mobDrop :: Mob -> Maybe Item
mobDrop = map mkItem <<< _.drop <<< getMobRecord <<< mobType
