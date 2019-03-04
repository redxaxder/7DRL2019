module Graphics.Sprite where

newtype Sprite = Sprite { offsetX :: Int, offsetY :: Int }

spriteAt :: Int -> Int -> Sprite
spriteAt offsetX offsetY = Sprite { offsetX, offsetY }

player :: Sprite
player = spriteAt 2 0

wall :: Sprite
wall = spriteAt 2 11

floor :: Sprite
floor = spriteAt 0 11

glitch :: Sprite
glitch = spriteAt 0 8
