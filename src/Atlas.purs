module Atlas
  ( addStitch
  , removeStitch
  , mkChart
  , Chart
  , ChartId (..)
  , Position (..)
  , move
  , LocalPosition
  , Atlas (..)
  , mkAtlas
  , addChart
  , getElement
  , getChart
  , find
  ) where


import Extra.Prelude

import Data.Lens (Lens', lens, set, view, (%~), (.~))
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Map (Map, lookup)
import Data.Map (size, delete, filter, insert, fromFoldable, empty, toUnfoldable) as M
import Data.Symbol (SProxy(..))
import Direction (Direction, opposite, localMove)
import Data.Array (mapWithIndex, concat)

addStitch :: forall a. Position -> Direction -> Position -> Atlas a -> Atlas a
addStitch source@(Position a) dir target@(Position b) atlas =
  let stitchA = End { localPosition: a.localPosition
                    , direction: dir
                    }
      stitchB = End { localPosition: b.localPosition
                    , direction: opposite dir
                    }
   in (chart a.chartId <<< stitches %~ M.insert stitchA target)
   <<< (chart b.chartId <<< stitches %~ M.insert stitchB source)
   $ atlas

removeStitch :: forall a. Position -> Direction -> Atlas a -> Atlas a
removeStitch source@(Position a) dir atlas =
  let target = move dir atlas source
   in removeHalfStitch source dir <<< removeHalfStitch target (opposite dir) $ atlas

removeHalfStitch :: forall a. Position -> Direction -> Atlas a -> Atlas a
removeHalfStitch (Position p) dir =
  let end = End { localPosition: p.localPosition
                , direction: dir
                }
   in chart p.chartId <<< stitches %~ M.delete end

move :: forall a. Direction -> Atlas a -> Position -> Position
move direction atlas (Position {chartId, localPosition}) =
  let (Chart c) = getChart chartId atlas
   in case lookup (End{localPosition, direction}) c.stitches of
           Just p -> p
           Nothing -> Position {chartId, localPosition: localMove direction localPosition }

getElement :: forall a. Position -> Atlas a -> a
getElement p a = view (element p) a

element :: forall a. Position -> Lens' (Atlas a) a
element (Position {chartId, localPosition}) = chart chartId <<< chartElem localPosition

setElement :: forall a. Position -> a -> Atlas a -> Atlas a
setElement p e = element p .~ e

modifyElement :: forall a. Position -> (a -> a) -> Atlas a -> Atlas a
modifyElement p f = element p %~ f

newtype Position = Position { chartId :: ChartId, localPosition :: LocalPosition }

derive instance eqPosition :: Eq Position
derive instance newtypePosition :: Newtype Position _
instance ordPosition :: Ord Position where
  compare (Position p1) (Position p2) =
    case compare p1.chartId p2.chartId of
         EQ -> compare p1.localPosition p2.localPosition
         x -> x

newtype Atlas a = Atlas { charts :: Map ChartId (Chart a)
                        , default :: Chart a
                        }

mkAtlas :: forall a. Chart a -> Atlas a
mkAtlas default = Atlas { default, charts: M.empty }

derive instance newtypeAtlas :: Newtype (Atlas a) _

chart :: forall a. ChartId -> Lens' (Atlas a) (Chart a)
chart cid = _Newtype <<< (lens get set)
  where
  get a = case lookup cid a.charts of
               Just c -> c
               Nothing -> a.default
  set a c = case lookup cid a.charts of
                 Just found -> a{ charts = M.insert cid c a.charts }
                 Nothing -> a { default = c }

getChart :: forall a. ChartId -> Atlas a -> Chart a
getChart chartId atlas = view (chart chartId) atlas

setChart :: forall a. ChartId -> Chart a -> Atlas a -> Atlas a
setChart chartId c atlas = set (chart chartId) c atlas

addChart :: forall a . Chart a
 -> Atlas a
 -> Tuple ChartId (Atlas a)
addChart c atlas@(Atlas a) =
  let chartId = nextId atlas
      a' = a { charts = M.insert chartId c a.charts }
   in Tuple chartId (Atlas a')

nextId :: forall a. Atlas a -> ChartId
nextId (Atlas {charts, default}) = ChartId $ M.size charts

newtype ChartId = ChartId Int

derive instance eqChartId :: Eq ChartId
derive instance ordChartId :: Ord ChartId
derive instance newtypeChartId :: Newtype ChartId _

newtype Chart a = Chart
  { elements :: Map LocalPosition a
  , stitches :: Map End Position
  , default  :: a
  , isDefault :: a -> Boolean
  }

mkChart :: forall a. Eq a
  => a
  -> Array (Array a)
  -> Chart a
mkChart default contents =
  let elements = M.fromFoldable $ concat <<< concat $
                   flip mapWithIndex contents $ \y row ->
                   flip mapWithIndex row $ \x t ->
                     if t == default
                       then []
                       else [Tuple (V {x,y}) t]
   in Chart { elements, stitches: M.empty, default, isDefault: \x -> x == default }

find :: forall a. (a -> Boolean) -> ChartId -> Atlas a -> Array (Tuple Position a)
find pred chartId a =
  let (Chart c) = getChart chartId a
   in do
      (Tuple localPosition v) <- M.toUnfoldable $ (M.filter pred) c.elements
      let p = Position { chartId, localPosition }
      pure (Tuple p v)

derive instance newtypeChart :: Newtype (Chart a) _

chartElem :: forall a. LocalPosition -> Lens' (Chart a) a
chartElem p = lens get set
  where
  get (Chart c) = case lookup p c.elements of
                       Just stuff -> stuff
                       Nothing -> c.default
  set (Chart c) stuff =
    let elements' = if (c.isDefault stuff)
         then M.delete p c.elements
         else M.insert p stuff c.elements
     in Chart c{ elements = elements' }

stitches :: forall a. Lens' (Chart a) (Map End Position)
stitches = _Newtype <<< prop (SProxy :: SProxy "stitches")

newtype End = End
  { localPosition :: LocalPosition
  , direction :: Direction
  }

derive instance eqEnd :: Eq End
derive instance ordEnd :: Ord End
derive instance newtypeEnd :: Newtype End _

type LocalPosition = Vector Int

------------------------------------------------------------------------------------------
-- Placeholders for partial maps
------------------------------------------------------------------------------------------


{-
getPlaceholders :: forall a. Position -> Atlas a -> Array Placeholder
getPlaceholders (Position p) (Atlas a) =
  let cid = p.chartId
   in filter (\x -> (un Position x.position).chartId == cid) a.placeholders

updateAtlas :: forall a. Position -> Atlas a -> Atlas a
updateAtlas pos a =
  let placeholders = getPlaceholders pos a
      actions = catMaybes $ flip map placeholders $ \p ->
                 flip map (findPlaceholder (opposite p.direction) a) $ \q ->
                   modify (force <<< attach p q)
   in execState (sequence_ actions) a
   where
   force :: forall b. Maybe b -> b
   force x = unsafePartial (fromJust x)

findPlaceholder :: forall a. Direction -> Atlas a -> Maybe Placeholder
findPlaceholder dir (Atlas a) = A.find (\x -> x.direction == dir) a.placeholders
-}
