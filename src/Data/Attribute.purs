module Data.Attribute where

import Extra.Prelude

newtype Attribute = Attribute String
derive instance eqAttribute :: Eq Attribute
derive instance ordAttribute :: Ord Attribute
derive instance newtypeAttribute :: Newtype Attribute _
