module Direction 
  ( Direction (..)
  , rotate
  , add
  , clockwise
  , widdershins
  , opposite
  , localMove
  )
where

import Prelude

import Extra.Math (Vector(..))

data Direction = U | D | L | R

derive instance eqDirection :: Eq Direction
derive instance ordDirection :: Ord Direction

rotate :: Int -> Direction -> Direction
rotate n f = fromInt $ (toInt f `z4` n)

z4 :: Int -> Int -> Int
z4 x y = (x + y) `mod` 4

add :: Direction -> Direction -> Direction
add a b = fromInt $ (toInt a `z4` toInt b)

toInt :: Direction -> Int
toInt R = 0
toInt U = 1
toInt L = 2
toInt D = 3

fromInt :: Int -> Direction
fromInt 0 = R
fromInt 1 = U
fromInt 2 = L
fromInt _ = D

clockwise :: Int
clockwise = 3

widdershins :: Int
widdershins = 1

opposite :: Direction -> Direction
opposite = rotate 2

localMove :: Direction -> Vector Int -> Vector Int
localMove dir (V p) = case dir of
  U -> V p{ y = p.y - 1 }
  D -> V p{ y = p.y + 1 }
  L -> V p{ x = p.x - 1 }
  R -> V p{ x = p.x + 1 }


