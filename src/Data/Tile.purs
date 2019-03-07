module Data.Tile where

import Extra.Prelude

import Data.Region (Region (..))
import Data.Sprite (Sprite, spriteAt)

data Tile = Wall Region | Floor Region

derive instance eqTile :: Eq Tile

blocksVision :: Tile -> Boolean
blocksVision (Wall _) = true
blocksVision _ = false

blocksMovement :: Tile -> Boolean
blocksMovement (Wall _) = true
blocksMovement _ = false

tileSprite :: Tile -> Sprite
tileSprite (Floor Kitchen) = spriteAt 0 0
tileSprite (Wall  Kitchen) = spriteAt 1 0
tileSprite (Floor Cave) = spriteAt 2 0
tileSprite (Wall  Cave) = spriteAt 3 0
