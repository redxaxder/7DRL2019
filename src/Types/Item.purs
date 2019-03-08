module Types.Item 
  ( module Types.Item
  , module Data.Item
  ) where

import Extra.Prelude

import Data.Item (ItemType, getItemRecord)
import Data.Sprite (Sprite)

newtype Item = Item { itemType :: ItemType } -- TODO: Add relevant item state here
derive instance newtypeItem :: Newtype Item _

mkItem :: ItemType -> Item
mkItem t = Item { itemType: t }

itemName :: Item -> String
itemName = _.name <<< getItemRecord <<< itemType

itemSprite :: Item -> Sprite
itemSprite = _.sprite <<< getItemRecord <<< itemType

itemType :: Item -> ItemType
itemType (Item {itemType: t}) = t

