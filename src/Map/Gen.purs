module Map.Gen where

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
import Random (Gen, Random, branch, runRandom', element)
import Types (GameState, MapGenHint, Placeholder, MapData, Tile (..), Region (..))
import Map.Data (staircase, startRoom, rooms)


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
genMapPiece p@{ position, direction, next: {rng} } atlas = 
  let { chart, exits, entrance } = flip runRandom' rng $ do 
        room <- element rooms
        load room direction
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

initMap :: Gen -> { atlas :: Atlas Tile, player :: Position, placeholders :: Map Position Placeholder }
initMap g =
  let { chart, exits } = flip runRandom' g $ load startRoom R
      errorRoom = mkChart (Wall Cave) [[Wall Cave]]
      atlasZero = mkAtlas errorRoom
      Tuple chartId atlas = addChart chart atlasZero
  in  { atlas
      , player: Position { chartId, localPosition: V {x: 1,y: 1} }
      , placeholders: Map.fromFoldable $ (\p -> Tuple p.position p) <$> exits chartId
      }

load
  :: MapData
  -> Direction
  -> Random { chart :: Chart Tile
     , exits :: ChartId -> Array Placeholder
     , entrance :: LocalPosition
     }
load {terrain} rotation = do
  let mapTokens = rotate rotation $ toMapTokens terrain
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
      entrance = case find (isEntrance <<< _.a) indexedMap of
        Nothing -> V { x:100, y:1000 } -- no entrance marker in template; just add one wherever
        Just {x,y} -> V { x, y }
      chart = mkChart (Wall Cave) tiles
  generators <- traverse (\_ -> branch) protoExits
  let exits cid = zipWith (mkExit cid) protoExits (( \rng -> {rng}) <$> generators )
  pure { chart, exits, entrance }

data MapToken = T Tile | Exit Direction | Entrance

isEntrance :: MapToken -> Boolean
isEntrance Entrance = true
isEntrance _ = false

addIndices :: forall a. Array (Array a) -> Array { x :: Int, y :: Int, a :: a }
addIndices arr = concat $ flip mapWithIndex arr \y row -> flip mapWithIndex row \x a ->
  { x, y, a }

getTile :: MapToken -> Tile
getTile (T a) = a
getTile _ = Floor Cave

toMapTokens :: Array String -> Array (Array MapToken)
toMapTokens rows = (map getMapToken <<< toCharArray) <$> rows

getMapToken :: Char -> MapToken
getMapToken '#' = T $ Wall Cave
getMapToken '^' = Exit U
getMapToken 'v' = Exit D
getMapToken '<' = Exit L
getMapToken '>' = Exit R
getMapToken '!' = Entrance
getMapToken _ =   T $ Floor Cave

defaultAtlas :: Atlas Tile
defaultAtlas = mkAtlas defaultChart

defaultChart :: Chart Tile
defaultChart = mkChart (Wall Cave) mempty
  
