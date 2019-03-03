module Init where

import Extra.Prelude

import Atlas (Atlas, Position(..), Chart, ChartId, mkAtlas, mkChart, addChart)
import Control.Monad.State (evalState)
import Control.Monad.State.Class (state, get)
import Data.Array (concat, mapWithIndex, catMaybes)
import Data.Map (Map, fromFoldable)
import Data.String.CodeUnits (toCharArray)
import Direction (Direction(..))
import Tile (Tile(..))
import Types (GameState, Item)


init :: GameState
init = flip evalState defaultAtlas $ do
  chartA <- state $ load partOne
  chartB <- state $ load partTwo
  atlas <- get
  pure { atlas
       , player: Position { chartId: chartB
                           , localPosition: V {x: 1,y: 1}
                           }
       , inventory: exampleInventory
       }

exampleInventory :: Map Char Item
exampleInventory = fromFoldable
  [ 'a' |> { name: "Apple" }
  , 'b' |> { name: "Banapple" }
  , 'c' |> { name: "Crabapple" }
  , 'd' |> { name: "Dapple" }
  ]

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
charToTile '^' = Tuple (Just U) Empty
charToTile 'v' = Tuple (Just D) Empty
charToTile '<' = Tuple (Just L) Empty
charToTile '>' = Tuple (Just R) Empty
charToTile _ =   Tuple Nothing Empty

defaultAtlas :: Atlas Tile
defaultAtlas = mkAtlas defaultChart

defaultChart :: Chart Tile
defaultChart = mkChart Wall mempty
