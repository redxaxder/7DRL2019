module MapGen where

import Extra.Prelude

import Data.Array (unsnoc, cons, uncons, concat, catMaybes, zipWith)
import Data.Foldable (find)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.String.CodeUnits (toCharArray)
import Control.Monad.Rec.Class (Step (..), tailRec)

import Atlas (Atlas, Chart, LocalPosition, Position(..), addStitch, ChartId, mkChart, addChart, mkAtlas)
import Direction (Direction(..))
import Direction as Dir
import Random (Gen)
import Tile (Tile(..))
import Types (GameState, MapGenHint, Placeholder)


expandMap :: GameState -> Maybe GameState
expandMap gs = if length visiblePlaceholders == 0 
  then Nothing
  else Just $ 
    let { atlas, toAdd } = tailRec go { atlas: gs.atlas, visible: visiblePlaceholders, toAdd: mempty }
        placeholders' = foldr Map.delete gs.placeholders (map _.position visiblePlaceholders)
                     <> (Map.fromFoldable $ map (\p -> Tuple p.position p) toAdd)
     in gs { atlas = atlas, placeholders = placeholders' }
  where
  visiblePlaceholders :: Array Placeholder
  visiblePlaceholders =
      catMaybes $ flip map gs.fov $ \{ absolute } -> Map.lookup absolute gs.placeholders

  go :: { atlas :: Atlas Tile, visible :: Array Placeholder, toAdd :: Array Placeholder }
     -> Step { atlas :: Atlas Tile, visible :: Array Placeholder, toAdd :: Array Placeholder } { atlas :: Atlas Tile, toAdd :: Array Placeholder }
  go { atlas, visible, toAdd } = case uncons visible of
    Nothing -> Done { atlas, toAdd }
    Just { head, tail } -> let { atlas: atlas', placeholders } = genMapPiece head atlas 
                           in Loop { atlas: atlas', visible: tail, toAdd: toAdd <> placeholders }

  

genMapPiece :: Placeholder -> Atlas Tile -> { atlas :: Atlas Tile, placeholders :: Array Placeholder }
genMapPiece p@{ position, direction, next: hint } atlas =
  let { terrain, next } = staircase hint
      { chart, exits, entrance } = load next terrain direction
      (Tuple chartId atlas') = addChart chart atlas
      entrancePosition = Position { chartId, localPosition: entrance }
      placeholders = exits chartId
      atlas'' = addStitch position direction entrancePosition atlas'
   in { atlas: atlas'', placeholders}

rotateLeft :: forall a. Array (Array a) -> Array (Array a)
rotateLeft xs = case sequence $ map unsnoc xs of
  Just pairs -> cons (map (_.last) pairs) (rotateLeft (map (_.init) pairs))
  Nothing -> mempty

rotate :: forall a. Direction -> Array (Array a) -> Array (Array a)
rotate d = repeatedly (Dir.toInt d) rotateLeft

type MapData = { terrain :: Array String, next :: Array MapGenHint }

initMap :: Gen -> { atlas :: Atlas Tile, player :: Position, placeholders :: Map Position Placeholder }
initMap g =
  let { terrain, next } = startRoom { rng: g }
      { chart, exits } = load next terrain R
      errorRoom = mkChart Wall [[Wall]]
      atlasZero = mkAtlas errorRoom
      Tuple chartId atlas = addChart chart atlasZero
  in  { atlas
      , player: Position { chartId, localPosition: V {x: 1,y: 1} }
      , placeholders: Map.fromFoldable $ (\p -> Tuple p.position p) <$> exits chartId
      }

load
  :: Array MapGenHint
  -> Array String
  -> Direction
  -> { chart :: Chart Tile
     , exits :: ChartId -> Array Placeholder
     , entrance :: LocalPosition
     }
load hints rows rotation =
  let mapTokens = rotate rotation $ toMapTokens rows
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
        Nothing -> V { x:100, y:1000 } -- no entrance marker in templace; just add one wherever
        Just {x,y} -> V { x, y }
      chart = mkChart Wall tiles
   in { chart, exits, entrance }

data MapToken = T Tile | Exit Direction | Entrance

isEntrance :: MapToken -> Boolean
isEntrance Entrance = true
isEntrance _ = false

addIndices :: forall a. Array (Array a) -> Array { x :: Int, y :: Int, a :: a }
addIndices arr = concat $ flip mapWithIndex arr \y row -> flip mapWithIndex row \x a ->
  { x, y, a }

getTile :: MapToken -> Tile
getTile (T a) = a
getTile _ = Empty

toMapTokens :: Array String -> Array (Array MapToken)
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
  
