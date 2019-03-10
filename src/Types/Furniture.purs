module Types.Furniture
  ( module Types.Furniture
  , module Data.Furniture
  ) where

import Extra.Prelude

import Data.Furniture (FurnitureType, getFurnitureRecord, counter)
import Data.Attribute (Attribute)
import Data.Sprite (Sprite)

newtype Furniture = Furniture { furnitureType :: FurnitureType } --TODO: add relevant furniture state here
derive instance newtypeFurniture :: Newtype Furniture _

mkFurniture :: FurnitureType -> Furniture
mkFurniture t = Furniture { furnitureType: t }

furnitureName :: Furniture -> String
furnitureName = _.name <<< getFurnitureRecord <<< furnitureType

furnitureSprite :: Furniture -> Sprite
furnitureSprite = _.sprite <<< getFurnitureRecord <<< furnitureType

furnitureType :: Furniture -> FurnitureType
furnitureType (Furniture {furnitureType: t}) = t
