module Map.Gen where

import Extra.Prelude

import Data.Array (uncons)
import Data.Map (Map)
import Data.Map as Map
import Control.Monad.Rec.Class (Step (..), tailRec)
import Partial.Unsafe (unsafePartial) --FIXME

import Atlas (Atlas, Chart, Position(..), addStitch, mkChart, addChart, mkAtlas)
import Direction (Direction(..))
import Random (Gen, runRandom', element)
import Types (GameState, Placeholder, Tile (..), Region (..), Furniture, getVisible, Item)
import Data.Maps (startRoom, roomsByRegion)
import Map.Load (load)


type ExpandPartial = { atlas :: Atlas Tile
                     , visible :: Array Placeholder
                     , toAdd :: Array Placeholder
                     , furniture :: Map Position Furniture
                     , items :: Map Position Item
                     }

type ExpandResult = { atlas :: Atlas Tile
                    , toAdd :: Array Placeholder
                    , furniture :: Map Position Furniture
                    , items :: Map Position Item
                    }

expandMap :: GameState -> Maybe GameState
expandMap gs = if length visiblePlaceholders == 0
  then Nothing
  else Just $
    let { atlas, toAdd, furniture, items } = tailRec go
          { atlas: gs.atlas
          , visible: visiblePlaceholders
          , toAdd: mempty
          , furniture: mempty
          , items: mempty
          }
        placeholders' = foldr Map.delete gs.placeholders (map _.position visiblePlaceholders)
                     <> (Map.fromFoldable $ map (\p -> Tuple p.position p) toAdd)
     in gs { atlas = atlas
           , placeholders = placeholders'
           , furniture = gs.furniture <> furniture
           , items = gs.items <> items
           }
  where
  visiblePlaceholders :: Array Placeholder
  visiblePlaceholders = _.a <$> getVisible gs.fov gs.placeholders

  go :: ExpandPartial -> Step ExpandPartial ExpandResult
  go { atlas, visible, toAdd, furniture, items } = case uncons visible of
    Nothing -> Done { atlas, toAdd, furniture, items }
    Just { head, tail } ->
      let { atlas: atlas', placeholders, furniture: f', items: i' } = genMapPiece head atlas
       in Loop { atlas: atlas'
               , visible: tail
               , toAdd: toAdd <> placeholders
               , furniture: furniture <> f'
               , items: items <> i'
               }

genMapPiece :: Placeholder -> Atlas Tile
  -> { atlas :: Atlas Tile
     , placeholders :: Array Placeholder
     , furniture :: Map Position Furniture
     , items :: Map Position Item
     }
genMapPiece p@{ position, direction, next: {rng} } atlas =
  let { chart, exits, entrance, furniture, items } = flip runRandom' rng $ do
        room <- unsafePartial $ element $ fromJust $ Map.lookup Cave roomsByRegion
        load room Cave direction
      (Tuple chartId atlas') = addChart chart atlas
      entrancePosition = Position { chartId, localPosition: entrance }
      placeholders = exits chartId
      atlas'' = addStitch position direction entrancePosition atlas'
   in { atlas: atlas'', placeholders, furniture: furniture chartId, items: items chartId }

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
