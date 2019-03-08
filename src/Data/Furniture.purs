module Data.Furniture
  ( FurnitureType
  , counter
  , furniture
  , furnitureByChar
  , getFurnitureRecord
  )
  where

import Extra.Prelude

import Data.Map (Map)
import Data.Map as Map

import Data.Attribute (Attribute (..))
import Data.Sprite (Sprite, spriteAt)

newtype FurnitureType = FurnitureType String
derive instance eqFurnitureType :: Eq FurnitureType
derive instance ordFurnitureType :: Ord FurnitureType

type FurnitureRecord =
  { furnitureType :: FurnitureType
  , name :: String
  , char :: Char
  , sprite :: Sprite
  , attributes :: Array Attribute
  }

f :: Char -> Int -> Int -> String -> Array String -> FurnitureRecord
f char x y name attributes =
  { char
  , sprite: spriteAt x y
  , furnitureType: FurnitureType name
  , name
  , attributes: Attribute <$> attributes
  }

counter :: FurnitureType
counter = FurnitureType "Counter"

furnitureRecords :: Array FurnitureRecord
furnitureRecords =
  [ { char: 'C', sprite: spriteAt 6 1, name: "Counter", attributes: [], furnitureType: counter}
  , f       'S'                   3 1        "Stove"                [ "heat" ]
  , f       'O'                   4 1        "Oven"                 [ "heat" ]
  , f       'B'                   5 1        "Cutting board"        []
  ]

furniture :: Array FurnitureType
furniture =  _.furnitureType <$> furnitureRecords

furnitureMap :: Map FurnitureType FurnitureRecord
furnitureMap = keyBy _.furnitureType furnitureRecords

getFurnitureRecord :: FurnitureType -> FurnitureRecord
getFurnitureRecord t = unsafeFromJust $ Map.lookup t furnitureMap
  -- As long as FurnitureType is only constructed in `furnitureRecords` this is safe

furnitureByChar :: Map Char FurnitureType
furnitureByChar = keyBy (_.char <<< getFurnitureRecord) furniture
