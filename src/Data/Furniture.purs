module Data.Furniture
  ( Furniture (..)
  , FurnitureType
  , counter
  , furniture
  , furnitureByChar
  , furnitureSprite
  , hasAttribute
  , mkFurniture
  )
  where

import Extra.Prelude

import Data.Map (Map)
import Data.Map as Map
import Partial.Unsafe (unsafePartial)


import Data.Attribute (Attribute (..))
import Data.Sprite (Sprite, spriteAt)


newtype Furniture = Furniture { name :: FurnitureType }
mkFurniture :: FurnitureType -> Furniture
mkFurniture name = Furniture { name }

derive instance newtypeFurniture :: Newtype Furniture _

instance showFurniture :: Show Furniture where
  show (Furniture {name}) = un FurnitureType name

newtype FurnitureType = FurnitureType String
derive instance eqFurnitureType :: Eq FurnitureType
derive instance ordFurnitureType :: Ord FurnitureType
derive instance newtypeFurnitureType :: Newtype FurnitureType _


type FurnitureRecord =
  { name :: FurnitureType
  , char :: Char
  , sprite :: Sprite
  , attributes :: Array Attribute
  }

f :: Char -> Int -> Int -> String -> Array String -> FurnitureRecord
f char x y name attributes =
  { char
  , sprite: spriteAt x y
  , name: FurnitureType name
  , attributes: Attribute <$> attributes
  }

counter :: FurnitureType
counter = FurnitureType "Counter"

furnitureRecords :: Array FurnitureRecord
furnitureRecords =
  [ { char: 'C', sprite: spriteAt 6 1, name: counter, attributes: [] }
  , f       'S'                   3 1        "Stove"              [ "heat" ]
  , f       'O'                   4 1        "Oven"               [ "heat" ]
  , f       'B'                   5 1        "Cutting board"      []
  ]

furniture :: Array FurnitureType
furniture =  _.name <$> furnitureRecords

furnitureMap :: Map FurnitureType FurnitureRecord
furnitureMap = keyBy _.name furnitureRecords

getFurnitureRecord :: FurnitureType -> FurnitureRecord
getFurnitureRecord name = unsafePartial $ fromJust $ Map.lookup name furnitureMap
  -- As long as FurnitureType is only constructed in `furnitureRecords` this is safe

furnitureSprite :: Furniture -> Sprite
furnitureSprite = _.sprite <<< getFurnitureRecord <<< _.name <<< un Furniture

furnitureByChar :: Map Char FurnitureType
furnitureByChar = keyBy (_.char <<< getFurnitureRecord) furniture

hasAttribute :: Furniture -> Attribute -> Boolean
hasAttribute (Furniture {name}) attr = elem attr (getFurnitureRecord name).attributes
