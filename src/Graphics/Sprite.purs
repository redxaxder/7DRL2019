module Graphics.Sprite where

import Tile (Tile (..))

newtype Sprite = Sprite { offsetX :: Int, offsetY :: Int }

spriteAt :: Int -> Int -> Sprite
spriteAt offsetX offsetY = Sprite { offsetX, offsetY }


tileSprite :: Tile -> Sprite
tileSprite Empty = spriteAt 2 0
tileSprite Wall =  spriteAt 3 0

player :: Sprite
player = spriteAt 4 0

glitch :: Sprite
glitch = spriteAt 4 3
