module Tile where

import Prelude

data Tile = Wall | Empty

derive instance eqTile :: Eq Tile

blocksVision :: Tile -> Boolean
blocksVision Wall = true
blocksVision _ = false

blocksMovement :: Tile -> Boolean
blocksMovement Wall = true
blocksMovement _ = false