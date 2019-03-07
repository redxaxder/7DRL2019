module Data.Tile where

import Extra.Prelude

import Data.Region (Region)

data Tile = Wall Region | Floor Region

derive instance eqTile :: Eq Tile

blocksVision :: Tile -> Boolean
blocksVision (Wall _) = true
blocksVision _ = false

blocksMovement :: Tile -> Boolean
blocksMovement (Wall _) = true
blocksMovement _ = false
