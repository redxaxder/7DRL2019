module Types.Mob 
  ( module Types.Mob
  , module Data.Mob
  ) where

import Extra.Prelude

import Data.Mob (MobType, getMobRecord)
import Data.Sprite (Sprite)
import Types.Item (Item, mkItem)

newtype Mob = Mob { mobType :: MobType, hp :: Int } -- TODO: Add relevant mob state here
derive instance newtypeMob :: Newtype Mob _

hit :: Mob -> Maybe Mob
hit m@(Mob {hp}) =
  let hp' = hp - 1
  in if hp' <= 0
  then Nothing
  else Just $ Mob $ (un Mob m) {hp = hp'}

mkMob :: MobType -> Mob
mkMob t = Mob { mobType: t
              , hp: _.hp $ getMobRecord t
              }

mobName :: Mob -> String
mobName = _.name <<< getMobRecord <<< mobType

mobSprite :: Mob -> Sprite
mobSprite = _.sprite <<< getMobRecord <<< mobType

mobType :: Mob -> MobType
mobType (Mob {mobType: t}) = t

mobDrop :: Mob -> Maybe Item
mobDrop = map mkItem <<< _.drop <<< getMobRecord <<< mobType
