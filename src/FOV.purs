module FOV where

import Extra.Prelude

import Data.Array (catMaybes, cons, nubBy, singleton, sortBy)
import Data.Array.NonEmpty (NonEmptyArray, cons', fromArray, head, toArray)
import Data.Filterable (filter)
import Data.Foldable (any)
import Data.Map (Map)
import Data.Map (fromFoldable, lookup) as Map
import Data.Map as M
import Direction (Direction(..), localMove)
import Atlas (Atlas, LocalPosition, Position, getElement, move)
import Tile (Tile, blocksVision)
import Math (abs)
import Types (GameState)

type ScreenPosition = { screen :: LocalPosition, absolute :: Position }

newtype QuadrantPosition = QuadrantPosition
  { quadrant :: Quadrant
  , xy :: LocalPosition
  , position :: Position
  }

derive instance ntQuadrantPosition :: Newtype QuadrantPosition _
derive instance eqQuadrantPosition :: Eq QuadrantPosition
derive instance ordQuadrantPosition :: Ord QuadrantPosition

toScreen :: QuadrantPosition -> ScreenPosition
toScreen (QuadrantPosition q) = { screen: q.xy, absolute: q.position }

data Quadrant = One | Two | Three | Four

derive instance eqQuadrant :: Eq Quadrant
derive instance ordQuadrant :: Ord Quadrant

quadrants :: NonEmptyArray Quadrant
quadrants = cons' One [Two, Three, Four]

scan :: Int -> GameState -> Results
scan distance gs =
  let
      player = gs.player
      atlas = gs.atlas
      contents = getElement player atlas
      init = flip addResult M.empty
               { pos: { screen: V {x:0,y:0}, absolute: player }
               , contents: singleton contents
               }
      subScans = map (scanQuadrant distance player atlas gs) quadrants
   in foldr1 (<<<) subScans init

type Result = { pos :: ScreenPosition, contents :: Array Tile }
type Results = M.Map ScreenPosition (Array Tile)

addResult :: Result -> Results -> Results
addResult r = M.insert r.pos r.contents

scanQuadrant
  :: Int -- distance to scan
  -> Position -- center
  -> Atlas Tile -- for looking up adjacent tiles
  -> GameState -- for items and mobs
  -> Quadrant -- quadrant to scan
  -> Results -- results so far
  -> Results
scanQuadrant distance position atlas gs quadrant acc =
    let (Tuple horz vert) = case quadrant of
          One   -> R |> U
          Two   -> L |> U
          Three -> L |> D
          Four  -> R |> D
        center = QuadrantPosition { position, quadrant, xy: zero}
        frontier = Frontier { cells: [center], shadows: [], horz, vert }
     in scanHelper distance [frontier] atlas gs acc

type Annotated = { q :: QuadrantPosition
  , screenPosition :: ScreenPosition
  , contents :: Tile
  , shadow :: Shadow
  , visible :: Boolean
  , blocker :: Boolean
  --, item :: Maybe Char
  }

scanHelper
  :: Int
  -> Array (Frontier QuadrantPosition)
  -> Atlas Tile
  -> GameState
  -> Results
  -> Results
scanHelper remaining frontiers atlas gs acc = if remaining > 0
          then scanHelper (remaining - 1) nextFrontiers atlas gs nextAcc
          else nextAcc
  where
  nextAcc = foldr addOne acc visibleCells
  toResult :: NonEmptyArray Annotated -> Result
  toResult x = { pos: (head x).screenPosition
               , contents: toArray $ map _.contents x
               }
  addOne :: NonEmptyArray Annotated
    -> M.Map ScreenPosition (Array Tile)
    -> M.Map ScreenPosition (Array Tile)
  addOne x m = addResult (toResult x) m
  shifted :: Array (Frontier QuadrantPosition)
  shifted = frontiers >>= shift atlas
  annotated :: Array (Frontier Annotated)
  annotated = flip map shifted $ \f@(Frontier x) ->
              map (annotate x.shadows) f
  annotate :: ShadowLine -> QuadrantPosition -> Annotated
  annotate shadows q =
    let shadow = project q
        screenPosition = toScreen q
        contents = getElement (unwrap q).position atlas
        --item = getItem (unwrap q).position gs
     in { q, screenPosition
        , contents, shadow
        , visible: not $ anyContains shadows shadow
        , blocker: blocksVision contents
        --, item: item
        }
  grouped :: Array (NonEmptyArray Annotated)
  grouped = groupBy' (comparing _.screenPosition) $
              annotated >>= \(Frontier x) -> x.cells
  visibleCells :: Array (NonEmptyArray Annotated)
  visibleCells = catMaybes $ map
     (fromArray <<< nubBy (comparing _.q) <<< filter _.visible <<< toArray) grouped
  overlapCount :: Map ScreenPosition Int
  overlapCount = Map.fromFoldable $ flip map grouped $ \grp ->
                 (head grp).screenPosition |> (length $ filter _.visible $ toArray grp)
  nextFrontiers :: Array (Frontier QuadrantPosition)
  nextFrontiers = flip map annotated $ \f@(Frontier x) ->
      let blockers = flip filter x.cells $ \cell ->
             cell.blocker || fromMaybe 0 (Map.lookup cell.screenPosition overlapCount) > 1
          newShadows = foldr insertShadow (map _.shadow blockers) x.shadows
       in Frontier x{ cells = map _.q $ (filter _.visible) x.cells
                    , shadows = newShadows
                    }

type Shadow = { start :: Number, end :: Number }

type ShadowLine = Array Shadow

insertShadow :: Shadow -> ShadowLine -> ShadowLine
insertShadow x xs =
  let new = fuse $ cons' x $ filter (overlaps x) xs
      nonoverlapping = filter (\y -> not (overlaps x y)) xs
   in cons new nonoverlapping
   where
   fuse :: NonEmptyArray Shadow -> Shadow
   fuse ys = let start :: Number
                 start = foldr1 min $ map (_.start) ys
                 end = foldr1 max $ map (_.end) ys
              in {start, end}

overlaps :: Shadow -> Shadow -> Boolean
overlaps x y = (x.start < y.start && x.end > y.start) || (x.start < y.end && x.end > y.end)

overlapsStrict :: Shadow -> Shadow -> Boolean
overlapsStrict x y = (x.start <= y.start && x.end >= y.start) || (x.start <= y.end && x.end >= y.end)

insertStrict :: Shadow -> ShadowLine -> ShadowLine
insertStrict x xs =
  let new = fuse $ cons' x $ filter (overlapsStrict x) xs
      nonoverlapping = filter (\y -> not (overlapsStrict x y)) xs
   in cons new nonoverlapping
   where
   fuse :: NonEmptyArray Shadow -> Shadow
   fuse ys = let start :: Number
                 start = foldr1 min $ map (_.start) ys
                 end = foldr1 max $ map (_.end) ys
              in {start, end}

anyContains :: ShadowLine -> Shadow -> Boolean
anyContains xs y = any (flip contains y) xs

contains :: Shadow -> Shadow -> Boolean
contains x y = (x.start <= y.start) && (x.end >= y.end)

transform :: forall a. Ring a => Quadrant -> Vector a -> Vector a
transform q p@(V {x,y}) = case q of
  One -> V { x: x, y: -y }
  Two -> V { x: -x, y: -y }
  Three -> V { x: -x, y }
  Four -> p

splitShadows :: Number -> ShadowLine -> Tuple ShadowLine ShadowLine
splitShadows d shadows = insertStrict { start: -2.0, end: d } shadows
  |> insertStrict { start: d, end: 2.0 } shadows

newtype Frontier a = Frontier
  { cells:: Array a
  , shadows:: ShadowLine
  , horz :: Direction
  , vert :: Direction
  }

derive instance functorFrontier :: Functor Frontier

absInt :: Int -> Int
absInt x | x >= 0 = x
         | otherwise = -x

shift :: Atlas Tile -> Frontier QuadrantPosition -> Array (Frontier QuadrantPosition)
shift atlas (Frontier{horz, vert, cells, shadows}) = finish (foldr f init
  (sortBy (comparing (map absInt <<< _.screen <<< toScreen)) cells))
  where
  _ = map toScreen cells
  finish x = x.result <> [Frontier { cells: x.cells , shadows: x.shadows , horz , vert }]
  moveq dir (QuadrantPosition q) =
    QuadrantPosition
    { quadrant: q.quadrant
    , xy: localMove dir q.xy
    , position: move dir atlas q.position
    }
  collides (QuadrantPosition q1) (QuadrantPosition q2) = q1.xy == q2.xy
  init = { result: []
         , cells: []
         , shadows
         , lastCell: Nothing
         }
  f next acc = let m1 = moveq horz next
                   m2 = moveq vert next
                   two _ = acc { lastCell = Just m2
                               , cells = acc.cells <> [m1,m2]
                               }
                   one _ = acc { lastCell = Just m2
                                          , cells = acc.cells <> [m2]
                                          }
                   split _ = let x = angle $ map (abs <<< toNumber) (unwrap m1).xy
                                 _ = acc.shadows
                                 (Tuple s1 s2) = splitShadows x acc.shadows
                              in  { result: acc.result <>
                                     [Frontier { cells: acc.cells
                                               , shadows: s1
                                               , horz
                                               , vert
                                               }]
                                  , cells: [m1,m2]
                                  , shadows: s2
                                  , lastCell: Just m2
                                  }
                              in case acc.lastCell of
                   Nothing -> two unit
                   Just prev ->
                     if prev == m1
                       then one unit
                       else if collides prev m1
                              then split unit
                              else two unit

angle :: forall a. Real a  => Vector a -> Number
angle v = let u = map toNumber v
            in u  ** (V {x: 1.0,y: -1.0}) / norm u

project :: QuadrantPosition -> Shadow
project (QuadrantPosition q) =
    let d = V { x: -0.5, y: 0.5 }
        w = map (abs <<< toNumber) q.xy
     in { start: angle (w + d)
        , end:   angle (w - d)
        }
