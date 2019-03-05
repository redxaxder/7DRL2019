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
import Types (MapGenHint, Placeholder)

attach :: forall a. Placeholder -> Placeholder -> Atlas a -> Maybe (Atlas a)
attach start end atlas@(Atlas a) = do
  guard (start.direction == (opposite end.direction))
  pure
    $ (addStitch start.position start.direction end.position)
    $ atlas

genMapPiece :: Placeholder -> MapGenHint -> Atlas Tile -> { atlas :: Atlas Tile, placeholders :: Array Placeholder }
genMapPiece p@{ position, direction } hint atlas = 
  let { terrain, next } = staircase hint
      { chart, exits } = load next terrain direction
      (Tuple chartId atlas') = addChart chart atlas
      placeholders = exits chartId
      atlas'' = attach p todo atlas'
   in { atlas: atlas'', placeholders}


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




load :: Array MapGenHint -> Array String -> Direction -> { chart :: Chart Tile, exits :: ChartId -> Array Placeholder, entrance :: { localPosition, Direction } }
load hints rows rotation =
  let mapTokens = toMapTokens rows
      indexedMap = addIndices mapTokens
      tiles = (map <<< map) getTile mapTokens
      protoExits = catMaybes $ flip map (indexedMap) $ \{ x, y, a } ->
                     case a of
                          Exit dir -> Just { localPosition: V {x,y}, dir }
                          _ -> Nothing
      mkExit chartId { dir, localPosition } next = 
        { direction: Dir.add dir rotation
        , position: Position { chartId, localPosition}
        , next
        }
      exits cid = zipWith (mkExit cid) protoExits hints

      entrance = case find (isEntrance <<< _.a) indexedMap of
        Nothing ->
        Just {x,y} -> \cid -> { }

      chart = mkChart Wall tiles
   in { chart, exits, entrance }

data MapToken = T Tile | Exit Direction | Entrance

addIndices :: forall a. Array (Array a) -> Array { x :: Int, y :: Int, a :: a }
addIndices = todo

getTile :: MapToken -> Tile
getTile (T a) = a
getTile _ = Empty

toMapTokens :: Array String -> Array Array MapToken
toMapTokens rows = (map getMapToken <<< toCharArray) <$> rows

getMapToken :: Char -> MapToken
getMapToken '#' = T Wall
getMapToken '^' = Exit U
getMapToken 'v' = Exit D
getMapToken '<' = Exit L
getMapToken '>' = Exit R
getMapToken '!' = Entrance
getMapToken _ =   T Empty

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
  
