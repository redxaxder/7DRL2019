module Types
  ( module Types
  , module Data.Attribute
  , module Data.Furniture
  , module Data.Item
  , module Data.Region
  , module Data.Sprite
  , module Data.Maps
  , module Data.Mob
  , module Data.Tile
  )
  where

import Extra.Prelude

import Data.Map (Map)

import Atlas (Atlas, Position)
import Data.Attribute (Attribute (..))
import Data.Furniture (Furniture (..), FurnitureName (..))
import Data.Item (Item (..), ItemName (..))
import Data.Maps (MapData (..))
import Data.Mob (Mob (..), MobName (..))
import Data.Region (Region (..))
import Data.Sprite (Sprite (..))
import Data.Tile (Tile (..))
import Direction (Direction)
import Random (Gen)

type GameState =
 { player :: Position
 , atlas :: Atlas Tile
 , inventory :: Map Char Item
 , items :: Map Position Item
 , placeholders :: Map Position Placeholder
 , fov :: FieldOfView
 , mobs :: Map Position Mob
 }

data UIRenderData = MainGame
  | StartScreen
  | InventoryScreen (Maybe {label :: Char, item :: Item})

type MapGenHint = { rng :: Gen }

type Placeholder = { position :: Position, direction :: Direction, next :: MapGenHint}

type FieldOfView = Array { screen :: Vector Int, absolute :: Position, tiles :: Array Tile }
