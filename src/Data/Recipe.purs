module Data.Recipe where

import Extra.Prelude

import Data.Array (sortBy)
import Data.Map (Map)
import Data.Map as Map
import Partial.Unsafe (unsafePartial)

import Data.Attribute (Attribute(..))
import Data.Furniture (FurnitureType, stringToFurnitureType)
import Data.Item (ItemType, stringToItemType, hasAttribute)


data CraftingMethod = Hand | ApplianceByName FurnitureType | ApplianceByAttribute Attribute -- maybe eventually...  | Tool Item

derive instance eqCraftingMethod :: Eq CraftingMethod
derive instance ordCraftingMethod :: Ord CraftingMethod

type RecipeInput = Either Attribute ItemType

type RecipeRecord  =
  { inputs :: Array RecipeInput
  , output :: ItemType
  , methods :: Map CraftingMethod Int
  }

a :: String -> RecipeInput
a s = Left (Attribute s)

i :: Partial => String -> RecipeInput
i s = Right (stringToItemType s)

f :: Partial => Array RecipeInput -> Array (Tuple CraftingMethod Int) -> String -> RecipeRecord
f inputs methods output = { inputs, output: stringToItemType output, methods: Map.fromFoldable methods }

suitable :: RecipeInput -> ItemType -> Boolean
suitable (Right it') it = it == it'
suitable (Left at) it = hasAttribute at it

distance :: Array ItemType -> Array RecipeInput -> Int
distance items inputs =
  inputs # countIf \input -> not (any $ suitable input) items

getRecipes :: Array ItemType -> Array RecipeRecord
getRecipes items = sortBy (comparing \r -> distance items r.inputs) recipeRecords

recipeCanUse :: RecipeRecord -> ItemType -> Boolean
recipeCanUse {inputs} it = any (\input -> suitable input it) inputs

attr :: Partial => String -> Int -> Tuple CraftingMethod Int
attr attrName turns = Tuple (ApplianceByAttribute $ Attribute attrName) turns
name :: Partial => String -> Int -> Tuple CraftingMethod Int
name furnitureName turns = Tuple (ApplianceByName $ stringToFurnitureType furnitureName) turns
hand :: Int -> Tuple CraftingMethod Int
hand turns = Tuple Hand turns

recipeRecords :: Array RecipeRecord
recipeRecords = unsafePartial $
  [ f [ a "tomato", a "onion", a "salt" ]
      [ attr "heat" 30 ]
      "soup"
  -- , f [ i "meat", i "diced onion", a "salt" ]
  --     [ name "stove" 30 ]
  --     "meat stew"
  , f [ a "meat", i "diced onion" ]
      [ name "oven" 30 ]
      "roast"
  , f [ i "chopped lettuce", i "diced tomato" ]
      [ hand 0 ]
      "tomato salad"
  , f [ i "diced onion", i "diced tomato", a "salt" ]
      [ hand 0 ]
      "tomato salad"
  , f [ i "deep lettuce" ]
      [ name "cutting board" 0 ]
      "chopped lettuce"
  , f [ i "whole tomato" ]
      [ name "cutting board" 0 ]
      "diced tomato"
  , f [ i "whole onion" ]
      [ name "cutting board" 0 ]
      "diced onion"
  ]
