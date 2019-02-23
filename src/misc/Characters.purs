module Characters where

import Prelude

import Data.Array (concat, length, range, zipWith)
import Data.Map (Map, fromFoldable)
import Data.Map (lookup) as Map
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Partial (crash)

tilesMap :: Map Char Int
tilesMap = fromFoldable tilesArray

tup :: Char -> Int -> Tuple Char Int
tup c i = Tuple c i

tileIndex :: Partial => Char -> Int
tileIndex i = case Map.lookup i tilesMap of
  Just x -> x
  Nothing -> crash

tilesArray :: Array (Tuple Char Int)
tilesArray = concat
  [ zipWith Tuple segment1 indices1
  , zipWith Tuple segment2 indices2
  , zipWith Tuple segment3 indices3
  ]
  where
        segment1 = toCharArray " !\"#$%&\'()*+,-./0123456789:;<=>?@[\\^_`{|}~"
        indices1 = range 0 (length segment1 - 1)
        segment2 = toCharArray "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        indices2 = range (32 * 3) (32 * 3 + length segment2 -1)
        segment3 = toCharArray "abcdefghijklmnopqrstuvwxyz"
        indices3 = range (33 * 4) (32 * 4 + length segment3 - 1)
