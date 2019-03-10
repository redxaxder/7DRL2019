module DistanceMap where

import Extra.Prelude

import Control.Monad.Rec.Class (tailRec, Step (..))
import Data.Array (filter, nub)
import Data.Map (Map)
import Data.Map as Map

import Direction (Direction (..))
import Atlas (Atlas, Position, move, getElement)
import Data.Tile (Tile, blocksMovement)

type DistanceMap = Map Position Int

makeDistanceMap :: Int -> Position -> Atlas Tile -> DistanceMap
makeDistanceMap range start atlas = tailRec go
  { acc: Map.singleton start 0
  , frontier: [start]
  , distance: 0
  }
  where
  go :: { acc :: DistanceMap, frontier :: Array Position, distance :: Int }
     -> Step { acc :: DistanceMap, frontier :: Array Position, distance :: Int } DistanceMap
  go { acc, frontier, distance } =
    if distance >= range
      then Done acc
      else let distance' = distance + 1
               frontier' = filter (\p -> not (Map.member p acc))
                 $ filter (\p -> not $ blocksMovement $ getElement p atlas)
                 $ nub $ do
                   p <- frontier
                   d <- [U, D, L, R]
                   pure $ move d atlas p
           in Loop { distance: distance'
                   , frontier: frontier'
                   , acc: acc <> Map.fromFoldable (flip Tuple distance' <$> frontier')
                   }


