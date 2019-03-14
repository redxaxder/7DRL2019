module Data.Recipe where

import Extra.Prelude

import Data.Array (sortBy, filter)
import Data.Map (Map)
import Data.Map as Map
import Partial.Unsafe (unsafePartial)

import Data.Attribute (Attribute(..))
import Data.Furniture (FurnitureType, stringToFurnitureType)
import Data.Furniture as Furniture
import Data.Item (stringToItemType, hasAttribute)
import Types.Item (Item, ItemType, itemType)

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

matchesMethod :: FurnitureType -> CraftingMethod -> Boolean
matchesMethod _ Hand = true
matchesMethod ft (ApplianceByName t) = ft == t
matchesMethod ft (ApplianceByAttribute at) = Furniture.hasAttribute ft at

distance :: Array ItemType -> Array RecipeInput -> Int
distance items inputs =
  inputs # countIf \input -> not (any $ suitable input) items

getRecipes :: Array ItemType -> Array RecipeRecord
getRecipes items = sortBy (comparing \r -> distance items r.inputs) recipeRecords

recipeCanUse :: RecipeRecord -> ItemType -> Boolean
recipeCanUse {inputs} it = any (\input -> suitable input it) inputs

mats :: RecipeRecord -> Array ItemType
mats r = map (unsafePartial \x -> case x of Right a -> a) (filter (either (const false) (const true)) r.inputs)

haveMats :: Map Char Item -> RecipeRecord -> Boolean
haveMats inventory recipe =
  let containsItem :: Map Char Item -> ItemType -> Boolean
      containsItem inv it = not null $ filter selectItem (Map.toUnfoldable inventory)
        where selectItem (Tuple char item) = itemType item == it
    in all (containsItem inventory) (mats recipe)

canCraft :: Maybe FurnitureType -> RecipeRecord -> Boolean
canCraft Nothing r = Map.member Hand r.methods
canCraft (Just ft) r = isJust $ find (matchesMethod ft) (Map.keys r.methods)

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
