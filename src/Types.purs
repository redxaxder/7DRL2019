module Types where

import Extra.Prelude

import Data.Map (Map)

import Atlas (Atlas, Position)
import Tile (Tile)
import Random (Gen)
import Direction (Direction)
import Graphics.Sprite (Sprite)

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

type Item = { name :: String }

type MapGenHint = { rng :: Gen }

type Placeholder = { position :: Position, direction :: Direction, next :: MapGenHint}

type FieldOfView = Array { screen :: Vector Int, absolute :: Position, tiles :: Array Tile }

type MapData = { terrain :: Array String }

type Mob = { name :: String, gfx :: Sprite }
