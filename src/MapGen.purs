module MapGen where

import Extra.Prelude

import Atlas (Atlas(..), Chart, Position(..), addStitch, ChartId, mkChart, addChart, mkAtlas)
import Control.MonadZero (guard)
import Data.Array (unsnoc, cons, catMaybes, concat, zipWith)
import Data.FunctorWithIndex (mapWithIndex)
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Direction (Direction(..), opposite)
import Direction (add) as Dir
import Random (Gen)
import Tile (Tile(..))
import Types (MapGenHint)

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
  
rotate :: (Array String) -> Direction -> Array String
rotate ar U = map fromCharArray $ rotateLeft (map toCharArray ar)
rotate ar L = map fromCharArray $ rotateLeft $ rotateLeft (map toCharArray ar)
rotate ar D = map fromCharArray $ rotateLeft $ rotateLeft $ rotateLeft (map toCharArray ar)
rotate ar _ = ar

type MapData = { terrain :: Array String, next :: Array MapGenHint }

initMap :: Gen -> { atlas :: Atlas Tile, player :: Position }
initMap g = 
  let { terrain, next } = startRoom { rng: g }
      { chart, exits } = load next terrain D
      errorRoom = mkChart Wall [[Wall]] 
      atlasZero = mkAtlas errorRoom
      Tuple chartId atlas = addChart chart atlasZero
  in  { atlas
      , player: Position { chartId, localPosition: V {x: 1,y: 1} }
      }




load :: Array MapGenHint -> Array String -> Direction -> { chart :: Chart Tile, exits :: ChartId -> Array Placeholder }
load hints rows rotation =
  let annotations = mapWithIndex getRow (rotate rows rotation)
      getRow y row = mapWithIndex (getCell y) (toCharArray row)
      getCell y x c = case charToTile c of
                           Tuple d tile -> { lp: V {x, y}, d, tile }
      tiles = (map <<< map) _.tile annotations
      protoExits = catMaybes $ flip map (concat annotations) $ \cell ->
                     case cell.d of
                          Just dir -> Just { dir, localPosition: cell.lp }
                          Nothing -> Nothing
      mkExit chartId {dir, localPosition} next = 
        { direction: Dir.add dir rotation
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
  
