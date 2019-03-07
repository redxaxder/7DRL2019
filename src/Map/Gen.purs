module Map.Gen where

import Extra.Prelude

import Data.Array (uncons, catMaybes)
import Data.Map (Map)
import Data.Map as Map
import Control.Monad.Rec.Class (Step (..), tailRec)
import Partial.Unsafe (unsafePartial) --FIXME

import Atlas (Atlas, Chart, Position(..), addStitch, mkChart, addChart, mkAtlas)
import Direction (Direction(..))
import Random (Gen, runRandom', element)
import Types (GameState, Placeholder, Tile (..), Region (..), Furniture)
import Data.Maps (startRoom, roomsByRegion)
import Map.Load (load)


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

genMapPiece :: Placeholder -> Atlas Tile
  -> { atlas :: Atlas Tile, placeholders :: Array Placeholder }
genMapPiece p@{ position, direction, next: {rng} } atlas =
  let { chart, exits, entrance, furniture } = flip runRandom' rng $ do
        room <- unsafePartial $ element $ fromJust $ Map.lookup Cave roomsByRegion
        load room Cave direction
      (Tuple chartId atlas') = addChart chart atlas
      entrancePosition = Position { chartId, localPosition: entrance }
      placeholders = exits chartId
      atlas'' = addStitch position direction entrancePosition atlas'
   in { atlas: atlas'', placeholders} --TODO: add furniture here

initMap :: Gen
  -> { atlas :: Atlas Tile
     , player :: Position
     , placeholders :: Map Position Placeholder
     , furniture :: Map Position Furniture
     }
initMap g =
  let { chart, exits, furniture } = flip runRandom' g $ load startRoom Kitchen R
      errorRoom = mkChart (Wall Cave) [[Wall Cave]]
      atlasZero = mkAtlas errorRoom
      Tuple chartId atlas = addChart chart atlasZero
  in  { atlas
      , player: Position { chartId, localPosition: V {x: 1,y: 1} }
      , placeholders: Map.fromFoldable $ (\p -> Tuple p.position p) <$> exits chartId
      , furniture: furniture chartId
      }

defaultAtlas :: Atlas Tile
defaultAtlas = mkAtlas defaultChart

defaultChart :: Chart Tile
defaultChart = mkChart (Wall Cave) mempty
