module Types
  ( module Types
  , module Data.Attribute
  , module Types.Furniture
  , module Types.Item
  , module Types.Mob
  , module Data.Region
  , module Data.Sprite
  , module Data.Maps
  , module Data.Tile
  )
  where

import Extra.Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Array (catMaybes)

import Atlas (Atlas, Position)
import Data.Attribute (Attribute (..))
import Data.Maps (MapData (..))
import Data.Region (Region (..))
import Data.Sprite (Sprite (..))
import Data.Tile (Tile (..))
import Direction (Direction)
import Random (Gen)
import Types.Furniture (FurnitureType, Furniture (..))
import Types.Item (Item (..), ItemType)
import Types.Mob (Mob (..), MobType)

type GameState =
 { player :: Position
 , atlas :: Atlas Tile
 , inventory :: Map Char Item
 , items :: Map Position Item
 , placeholders :: Map Position Placeholder
 , fov :: FieldOfView
 , mobs :: Map Position Mob
 , furniture :: Map Position Furniture
 , logevents :: Array LogEvent -- log all the events
 }

-- TODO: where should this live?
getVisible :: forall a. FieldOfView -> Map Position a -> Array { a :: a, screen :: Vector Int }
getVisible fov m = catMaybes $ flip map fov $ \{ screen, absolute } ->
      map (\a -> { a, screen }) $ Map.lookup absolute m

data UIRenderData = MainGame
  | StartScreen
  | InventoryScreen (Maybe {label :: Char, item :: Item})

type MapGenHint = { rng :: Gen, region :: Region }

type Placeholder = { position :: Position, direction :: Direction, next :: MapGenHint}

type FieldOfView = Array { screen :: Vector Int, absolute :: Position, tiles :: Array Tile }

data LogEvent = ItemEvent Item | CombatEvent Mob | MonsterKilledEvent Mob
