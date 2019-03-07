module Graphics.Sprite where

--import Canvas (Sprite(..))
import Tile (Tile(..))

import Types (Sprite (..))

spriteAt :: Int -> Int -> Sprite
spriteAt offsetX offsetY = Sprite { offsetX, offsetY }


tileSprite :: Tile -> Sprite
tileSprite Empty = spriteAt 2 0
tileSprite Wall =  spriteAt 3 0

player :: Sprite
player = spriteAt 4 0

glitch :: Sprite
glitch = spriteAt 4 3

bananamatronicHusk :: Sprite
bananamatronicHusk = spriteAt 0 1