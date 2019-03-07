module Data.Furniture where

import Extra.Prelude

import Data.Map (Map)

import Graphics.Sprite (spriteAt)
import Types (Furniture (..), FurnitureName (..), Attribute (..), Sprite)


f :: Char -> Int -> Int -> String -> Array String -> Furniture
f char x y name attributes = Furniture
  { char
  , sprite: spriteAt x y
  , name: FurnitureName name
  , attributes: Attribute <$> attributes
  }

counter :: Furniture
counter = f 'C' 6 1 "Counter" []

furniture :: Array Furniture
furniture =
  [ f 'S' 3 1 "Stove" [ "heat" ]
  , f 'O' 4 1 "Oven"  [ "heat" ]
  , f 'B' 5 1 "Cutting board" []
  , counter
  ]

furnitureSprite :: Furniture -> Sprite
furnitureSprite = _.sprite <<< un Furniture

furnitureByChar :: Map Char Furniture
furnitureByChar = keyBy (_.char <<< un Furniture) furniture
