module Data.Item
  ( items
  , orderable
  , getItemRecord
  , itemByChar
  , itemName
  , itemNameFromType
  , itemSprite
  , stringToItemType
  , hasAttribute
  ) where

import Extra.Prelude

import Data.Array (filter)
import Data.Array.NonEmpty (NonEmptyArray, fromArray)
import Data.Foldable (find)
import Data.Map (Map)
import Data.Map as Map

import Data.Attribute (Attribute(..))
import Data.Sprite (Sprite, spriteAt)
import Data.String.Common (toLower)
import Types.Item (Item, ItemType(..), itemType)

type ItemRecord =
  { itemType :: ItemType
  , name :: String
  , char :: Maybe Char
  , sprite :: Sprite
  , attributes :: Array Attribute
  , canOrder :: Boolean
  }


i :: Char -> Int -> Int -> String -> Array String -> ItemRecord
i c = mkItemRecord false (Just c)

j :: Int -> Int -> String -> Array String -> ItemRecord
j = mkItemRecord false Nothing

k :: Int -> Int -> String -> Array String -> ItemRecord
k = mkItemRecord true Nothing

mkItemRecord :: Boolean -> Maybe Char -> Int -> Int -> String -> Array String -> ItemRecord
mkItemRecord canOrder char x y name attributes =
  { itemType: ItemType name
  , char
  , name
  , sprite: spriteAt x y
  , attributes: Attribute <$> attributes
  , canOrder
  }

itemRecords :: Array ItemRecord
itemRecords =
  [ k     1 2 "Tomato salad"     mempty
  , k     2 2 "Soup"             mempty
  , k     3 2 "Roast"            mempty
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

itemName :: Item -> String
itemName = _.name <<< getItemRecord <<< itemType

itemNameFromType :: ItemType -> String
itemNameFromType = _.name <<< getItemRecord

itemSprite :: Item -> Sprite
itemSprite = _.sprite <<< getItemRecord <<< itemType

orderable :: NonEmptyArray ItemType
orderable = unsafeFromJust $ fromArray $ filter (_.canOrder <<< getItemRecord) items

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
