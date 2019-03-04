module Types where

import Extra.Prelude

import Data.Map (Map)

import Atlas (Atlas, Position)
import Tile (Tile)
import Random (Gen)

type GameState =
 { player :: Position
 , atlas :: Atlas Tile
 , inventory :: Map Char Item
 }

data UIRenderData = MainGame
  | StartScreen
  | InventoryScreen (Maybe {label :: Char, item :: Item})

type Item = { name :: String }


type MapGenHint = { rng :: Gen }