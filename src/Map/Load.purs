module Map.Load 
  ( load
  ) where

import Extra.Prelude

import Data.Array (unsnoc, cons, concat, catMaybes, zipWith)
import Data.Foldable (find)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Map (Map)
import Data.Map as Map
import Data.String.CodeUnits (toCharArray)

import Atlas (Chart, LocalPosition, Position(..), ChartId, mkChart)
import Data.Furniture (furnitureByChar, mkFurniture)
import Data.Maps (getTerrain)
import Direction (Direction(..))
import Direction as Dir
import Random (Random, branch)
import Types (Placeholder, MapData, Tile (..), Region (..), Furniture)

load
  :: MapData
  -> Region
  -> Direction
  -> Random { chart :: Chart Tile
            , exits :: ChartId -> Array Placeholder
            , entrance :: LocalPosition
            , furniture :: ChartId -> Map Position Furniture
            }
load mapData region rotation = do
  let mapTokens = rotate rotation $ toMapTokens region (getTerrain mapData)
      indexedMap = addIndices mapTokens
      tiles = (map <<< map) (getTile region) mapTokens
      protoExits = catMaybes $ flip map (indexedMap) $ \{ x, y, a } ->
                     case a of
                          Exit dir -> Just { localPosition: V {x,y}, dir }
                          _ -> Nothing
      mkExit chartId { dir, localPosition } next =
        { direction: Dir.add dir rotation
        , position: Position { chartId, localPosition}
        , next
        }
      entrance = case find ((eq Entrance) <<< _.a) indexedMap of
        Nothing -> V { x:100, y:1000 } -- no entrance marker in template; just add one wherever
        Just {x,y} -> V { x, y }
      chart = mkChart (Wall Cave) tiles
      furniture = placeFurniture indexedMap
  generators <- traverse (\_ -> branch) protoExits
  let exits cid = zipWith (mkExit cid) protoExits (( \rng -> {rng, region}) <$> generators )
  pure $ { chart, exits, entrance, furniture }

placeFurniture :: IxArray MapToken -> ChartId -> Map Position Furniture
placeFurniture mapTokens chartId = Map.fromFoldable $ catMaybes
  $ mapTokens # map \{x,y,a} -> do
    c <- getChar a
    fType <- Map.lookup c furnitureByChar
    pure $ Tuple (Position { chartId, localPosition: V{x,y} }) (mkFurniture fType)

rotateLeft :: forall a. Array (Array a) -> Array (Array a)
rotateLeft xs = case sequence $ map unsnoc xs of
  Just pairs -> cons (map (_.last) pairs) (rotateLeft (map (_.init) pairs))
  Nothing -> mempty

rotate :: forall a. Direction -> Array (Array a) -> Array (Array a)
rotate d = repeatedly (Dir.toInt d) rotateLeft

data MapToken = T Tile | Exit Direction | Entrance | Character Char
derive instance eqMapToken :: Eq MapToken

getChar :: MapToken -> Maybe Char
getChar (Character c) = Just c
getChar _ = Nothing

type IxArray a = Array { x :: Int, y :: Int, a :: a }

addIndices :: forall a. Array (Array a) -> IxArray a
addIndices arr = concat $
 flip mapWithIndex arr \y row ->
 flip mapWithIndex row \x a -> { x, y, a }

getTile :: Region -> MapToken -> Tile
getTile _ (T a) = a
getTile r _ = Floor r

toMapTokens :: Region -> Array String -> Array (Array MapToken)
toMapTokens r rows = (map (getMapToken r) <<< toCharArray) <$> rows

getMapToken :: Region -> Char -> MapToken
getMapToken r '#' = T $ Wall r
getMapToken _ '^' = Exit U
getMapToken _ 'v' = Exit D
getMapToken _ '<' = Exit L
getMapToken _ '>' = Exit R
getMapToken _ '!' = Entrance
getMapToken r '.' = T $ Floor r
getMapToken _  c  = Character c
