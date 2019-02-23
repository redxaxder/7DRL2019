module Init where

import Extra.Prelude

import Control.Monad.State (evalState)
import Control.Monad.State.Class (state, get)
import Data.String.CodeUnits (toCharArray)
import Data.Array (concat, mapWithIndex, catMaybes)

import Atlas
  ( Atlas
  , Position(..)
  , Chart
  , ChartId
  , mkAtlas
  , mkChart
  , addChart
  )
import Direction (Direction (..))
import Types (GameState)
import Tile (Tile (..))


init :: GameState
init = flip evalState defaultAtlas $ do
  chartA <- state $ load partOne
  chartB <- state $ load partTwo
  atlas <- get
  pure { atlas
       , player: Position { chartId: chartB
                           , localPosition: V {x: 1,y: 1}
                           }
       }


partOne :: Array String
partOne =
  [ "#########"
  , "#.......#"
  , "#..#v#..#"
  , "#..###..#"
  , "#.......#"
  , "#########"
  ]

partTwo :: Array String
partTwo =
  [ "###^###"
  , "#.....#"
  , "#.....#"
  , "#######"
  ]


load :: Array String -> Atlas Tile -> Tuple ChartId (Atlas Tile)
load rows atlas =
  let annotations = mapWithIndex getRow rows
      getRow y row = mapWithIndex (getCell y) (toCharArray row)
      getCell y x c = case charToTile c of
                           Tuple p t -> { k: V {x, y}, p, t }
      tiles = (map <<< map) _.t annotations
      placeholders = catMaybes $ flip map (concat annotations) $ \cell ->
                     case cell.p of
                          Just p -> Just (Tuple cell.k p)
                          Nothing -> Nothing
      chart = mkChart Wall tiles
   in addChart chart placeholders atlas

charToTile :: Char -> Tuple (Maybe Direction) Tile
charToTile '#' = Tuple Nothing Wall
charToTile '^' = Tuple (Just Up) Empty
charToTile 'v' = Tuple (Just Down) Empty
charToTile '<' = Tuple (Just Left) Empty
charToTile '>' = Tuple (Just Right) Empty
charToTile _ =   Tuple Nothing Empty

defaultAtlas :: Atlas Tile
defaultAtlas = mkAtlas defaultChart

defaultChart :: Chart Tile
defaultChart = mkChart Wall mempty
