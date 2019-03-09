module Data.Item
  ( ItemType
  , items
  , getItemRecord
  , itemByChar
  , stringToItemType
  , hasAttribute
  ) where

import Extra.Prelude

import Data.Foldable (find)
import Data.Map (Map)
import Data.Map as Map

import Data.Attribute (Attribute(..))
import Data.Sprite (Sprite, spriteAt)
import Data.String.Common (toLower)

newtype ItemType = ItemType String
derive instance eqItemName :: Eq ItemType
derive instance ordItemName :: Ord ItemType

type ItemRecord =
  { itemType :: ItemType
  , name :: String
  , char :: Maybe Char
  , sprite :: Sprite
  , attributes :: Array Attribute
  }

i :: Char -> Int -> Int -> String -> Array String -> ItemRecord
i c = mkItemRecord (Just c)

j :: Int -> Int -> String -> Array String -> ItemRecord
j = mkItemRecord Nothing

mkItemRecord :: Maybe Char -> Int -> Int -> String -> Array String -> ItemRecord
mkItemRecord char x y name attributes =
  { itemType: ItemType name
  , char
  , name
  , sprite: spriteAt x y
  , attributes: Attribute <$> attributes
  }

itemRecords :: Array ItemRecord
itemRecords =
  [ j     1 2 "Tomato salad"     mempty
  , j     2 2 "Soup"             mempty
  , j     3 2 "Roast"            mempty
  , j     4 2 "Whole tomato"     [ "tomato" ]
  , j     5 2 "Whole onion"      [ "onion" ]
  , j     6 2 "Raw meat"         [ "meat" ]
  , j     7 2 "Deep lettuce"     [ "lettuce" ]
  , j     1 3 "Diced tomato"     [ "tomato" ]
  , j     2 3 "Diced onion"      [ "onion" ]
  , j     3 3 "Chopped lettuce"  [ "lettuce" ]
  , i 's' 2 1 "Cave salt"        [ "salt" ]
  ]

items :: Array ItemType
items = _.itemType <$> itemRecords

itemMap :: Map ItemType ItemRecord
itemMap = keyBy _.itemType itemRecords

getItemRecord :: ItemType -> ItemRecord
getItemRecord t = unsafeFromJust $ Map.lookup t itemMap
  -- this is safe as long as ItemType is only ever constructed within itemRecords

itemByName :: Map ItemType ItemRecord
itemByName = keyBy _.itemType itemRecords

itemByChar :: Map Char ItemType
itemByChar = keyBy' (_.char <<< getItemRecord) items

hasAttribute :: Attribute -> ItemType -> Boolean
hasAttribute attr item = elem attr (getItemRecord item).attributes

stringToItemType :: Partial => String -> ItemType
stringToItemType name = fromJust $ items # find \item -> toLower (getItemRecord item).name == toLower name