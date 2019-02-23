module Direction where

import Prelude

import Extra.Math (Vector (..))


data Direction = Up | Down | Left | Right

derive instance eqDirection :: Eq Direction
derive instance ordDirection :: Ord Direction

rotate :: Int -> Direction -> Direction
rotate n f = fromInt $ ((toInt f) + n) `mod` 4
  where
  toInt Right = 0
  toInt Up = 1
  toInt Left = 2
  toInt Down = 3
  fromInt 0 = Right
  fromInt 1 = Up
  fromInt 2 = Left
  fromInt _ = Down

clockwise :: Int
clockwise = 3

widdershins :: Int
widdershins = 1

opposite :: Direction -> Direction
opposite = rotate 2

localMove :: Direction -> Vector Int -> Vector Int
localMove dir (V p) = case dir of
  Up    -> V p{ y = p.y - 1 }
  Down  -> V p{ y = p.y + 1 }
  Left  -> V p{ x = p.x - 1 }
  Right -> V p{ x = p.x + 1 }


