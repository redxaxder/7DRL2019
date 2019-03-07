module Data.Sprite where

newtype Sprite = Sprite { offsetX :: Int, offsetY :: Int }

spriteAt :: Int -> Int -> Sprite
spriteAt offsetX offsetY = Sprite { offsetX, offsetY }

player :: Sprite
player = spriteAt 4 0

glitch :: Sprite
glitch = spriteAt 4 3
