module Types where

import Extra.Prelude

import Data.Map (Map)

import Atlas (Atlas, Position)
import Tile (Tile)
import Random (Gen)
import Direction (Direction)

type GameState =
 { player :: Position
 , atlas :: Atlas Tile
 , inventory :: Map Char Item
 , items :: Map Position Item
 , placeholders :: Map Position Placeholder
 , fov :: FieldOfView
 }

data UIRenderData = MainGame
  | StartScreen
  | InventoryScreen (Maybe {label :: Char, item :: Item})

--type Item = { name :: String }

type MapGenHint = { rng :: Gen }

type Placeholder = { position :: Position, direction :: Direction, next :: MapGenHint}

type FieldOfView = Array { screen :: Vector Int, absolute :: Position, tiles :: Array Tile }

type MapData = { terrain :: Array String }


newtype Sprite = Sprite { offsetX :: Int, offsetY :: Int }

newtype Attribute = Attribute String
derive instance eqAttribute :: Eq Attribute

newtype ItemName = ItemName String
derive instance eqItemName :: Eq ItemName
derive instance ordItemName :: Ord ItemName
derive instance newtypeItemName :: Newtype ItemName _

newtype Item = Item
  { name :: ItemName
  , char :: Maybe Char
  , sprite :: Sprite
  , attributes :: Array Attribute
  }

derive instance newtypeItem :: Newtype Item _

newtype FurnitureName = FurnitureName String
derive instance eqFurnitureName :: Eq FurnitureName
derive instance ordFurnitureName :: Ord FurnitureName
derive instance newtypeFurnitureName :: Newtype FurnitureName _

newtype Furniture = Furniture
  { name :: FurnitureName
  , char :: Char
  , sprite :: Sprite
  , attributes :: Array Attribute
  }
derive instance newtypeFurniture :: Newtype Furniture _
