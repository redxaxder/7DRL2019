module Types.Item where

import Extra.Prelude

newtype ItemType = ItemType String
derive instance eqItemName :: Eq ItemType
derive instance ordItemName :: Ord ItemType

newtype Item = Item { itemType :: ItemType } -- TODO: Add relevant item state here
derive instance eqItem :: Eq Item
derive instance newtypeItem :: Newtype Item _

mkItem :: ItemType -> Item
mkItem t = Item { itemType: t }

itemType :: Item -> ItemType
itemType (Item {itemType: t}) = t
