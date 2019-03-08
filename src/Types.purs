module Types
  ( module Types
  , module Data.Attribute
  , module Types.Furniture
  , module Types.Item
  , module Data.Region
  , module Data.Sprite
  , module Data.Maps
  , module Data.Mob
  , module Data.Tile
  )
  where

import Extra.Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Array (catMaybes)

import Atlas (Atlas, Position)
import Data.Attribute (Attribute (..))
import Types.Furniture (FurnitureType, Furniture (..))
import Types.Item (Item (..), ItemType)
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
 , furniture :: Map Position Furniture
 , logevent :: Maybe LogEvent -- Nothing == nothing to log
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

data LogEvent = ItemEvent Item | CombatEvent Mob
