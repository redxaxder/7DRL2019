module Data.Item 
  ( items
  , itemName
  , itemSprite
  , itemsByName
  , itemsByChar
  )
  where

import Extra.Prelude

import Data.Map (Map)

import Graphics.Sprite (spriteAt)
import Types (Item (..), ItemName (..), Attribute (..), Sprite)

i :: Char -> Int -> Int -> String -> Array String -> Item
i c = mkItem (Just c)

j :: Int -> Int -> String -> Array String -> Item
j = mkItem Nothing

mkItem :: Maybe Char -> Int -> Int -> String -> Array String -> Item
mkItem char x y name attributes = Item
  { name: ItemName name
  , char
  , sprite: spriteAt x y
  , attributes: Attribute <$> attributes
  }

items :: Array Item
items =
  [ j     1 2 "Tomato salad"     mempty
  , j     2 2 "Soup"             mempty
  , j     3 2 "Roast"            mempty
  , j     4 2 "Tomato"           [ "tomato" ]
  , j     5 2 "Onion"            [ "onion" ]
  , j     6 2 "Raw meat"         [ "meat" ]
  , j     1 3 "Diced tomato"     [ "tomato" ]
  , j     2 3 "Diced onion"      [ "onion" ]
  , j     3 3 "Chopped lettuce"  [ "lettuce" ]
  , i 'l' 1 1 "Deep lettuce"     [ "lettuce" ]
  , i 's' 2 1 "Cave salt"        mempty
  ]

itemsByChar :: Map Char Item
itemsByChar = keyBy' (_.char <<< un Item) items

itemsByName :: Map ItemName Item
itemsByName = keyBy itemName items

itemName :: Item -> ItemName
itemName = _.name <<< un Item

itemSprite :: Item -> Sprite
itemSprite = _.sprite <<< un Item
