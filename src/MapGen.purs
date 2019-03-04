module MapGen where

import Extra.Prelude

import Web.HTML.Event.EventTypes (offline)
import Control.MonadZero (guard)
import Data.FunctorWithIndex (mapWithIndex)
import Data.String.CodeUnits (toCharArray)
import Data.Array (unsnoc, cons, catMaybes, concat, zipWith)

import Tile (Tile(..))
import Atlas (Atlas(..), Chart, Position(..), LocalPosition, addStitch, ChartId, mkChart, addChart, mkAtlas)
import Direction (Direction (..), opposite, add)
import Types (MapGenHint)
import Random (Gen)

type Placeholder = { position :: Position, direction :: Direction, next :: MapGenHint}
attach :: forall a. Placeholder -> Placeholder -> Atlas a -> Maybe (Atlas a)
attach start end atlas@(Atlas a) = do
  guard (start.direction == (opposite end.direction))
  pure
    $ (addStitch start.position start.direction end.position)
    $ atlas

genMapPiece :: Placeholder -> MapGenHint -> Atlas Tile -> { atlas :: Atlas Tile, placeholders :: Array Placeholder }
genMapPiece { position, direction } { rng } atlas = todo


rotateLeft :: forall a. Array (Array a) -> Array (Array a)
rotateLeft xs = case sequence $ map unsnoc xs of
  Just pairs -> cons (map (_.last) pairs) (rotateLeft (map (_.init) pairs))
  Nothing -> mempty
  

type MapData = { terrain :: Array String, next :: Array MapGenHint }

initMap :: Gen -> { atlas :: Atlas Tile, player :: Position }
initMap g = 
  let { terrain, next } = startRoom { rng: g }
      { chart, exits } = load next terrain R
      errorRoom = mkChart Wall [[Wall]] 
      atlasZero = mkAtlas errorRoom
      Tuple chartId atlas = addChart chart atlasZero
  in  { atlas
      , player: Position { chartId, localPosition: V {x: 1,y: 1} }
      }




load :: Array MapGenHint -> Array String -> Direction -> { chart :: Chart Tile, exits :: ChartId -> Array Placeholder }
load hints rows rotation =
  let annotations = mapWithIndex getRow rows
      getRow y row = mapWithIndex (getCell y) (toCharArray row)
      getCell y x c = case charToTile c of
                           Tuple d tile -> { lp: V {x, y}, d, tile }
      tiles = (map <<< map) _.tile annotations
      protoExits = catMaybes $ flip map (concat annotations) $ \cell ->
                     case cell.d of
                          Just dir -> Just { dir, localPosition: cell.lp }
                          Nothing -> Nothing
      mkExit chartId {dir, localPosition} next = 
        { direction: add dir rotation
        , position: Position { chartId, localPosition}
        , next
        }
      exits cid = zipWith (mkExit cid) protoExits hints
      chart = mkChart Wall tiles
   in { chart, exits }

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

startRoom :: MapGenHint -> MapData
startRoom {rng} = 
  { terrain:
      [ "##########"
      , "#......#^#"
      , "#....#...#"
      , "#........#"
      , "#...#....#"
      , "#........#"
      , "##########"
      ]
  , next: [{rng}]
  }

sampleMap1 :: MapGenHint -> MapData
sampleMap1 {rng} = 
  { terrain:
      [ "######^######"
      , "#####...#####"
      , "###.......###"
      , "#...........>"
      , "!...........#"
      , "###.#.....###"
      , "#####...#####"
      , "######v######"
      ]
  , next: [{rng},{rng},{rng}]
  }
  
sampleHall1 :: MapGenHint -> MapData
sampleHall1 {rng} = 
  { terrain: 
    [ "#######"
    , "!.....>"
    , "#######"
    ]
  , next: [{rng}]
  }

staircase :: MapGenHint -> MapData  
staircase {rng} = 
  { terrain:
    [ "###"
    , "<.#"
    , "#.#"
    , "#!#"
    , "###"
    ]
  , next: [{rng}]
  }  
  