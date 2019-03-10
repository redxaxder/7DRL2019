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
import Data.Furniture (furnitureByChar)
import Data.Maps (getTerrain)
import Direction (Direction(..))
import Direction as Dir
import Random (Random, branch, chance, element)
import Types (Furniture, Item, MapData, MobType, Placeholder, Region(..), Tile(..), Mob)
import Types.Mob (mkMob)
import Types.Furniture (mkFurniture)
import Types.Item (mkItem)
import Data.Item (itemByChar)
import Data.Mob (mobs)

load
  :: MapData
  -> Region
  -> Direction
  -> Random { chart :: Chart Tile
            , exits :: ChartId -> Array Placeholder
            , entrance :: LocalPosition
            , furniture :: ChartId -> Map Position Furniture
            , items :: ChartId -> Map Position Item
            , mobs :: ChartId -> Array Mob
            }
load mapData region rotation = do
  mapTokens <- rotate rotation
    <$> (sequence <<< map sequence) (toMapTokens region (getTerrain mapData))
  let indexedMap = addIndices mapTokens
      tiles = (map <<< map) (getTile region) mapTokens
      protoExits = catMaybes $ flip map indexedMap $ \{ x, y, a } ->
                     case a of
                          Exit dir -> Just { localPosition: V {x,y}, dir }
                          _ -> Nothing
      mkExit chartId { dir, localPosition } next =
        { direction: Dir.add dir rotation
        , position: Position { chartId, localPosition}
        , next
        }
      protoMobs = catMaybes $ flip map indexedMap $ \{ x, y, a } ->
                    case a of
                         Monster mt -> Just { localPosition: V {x,y}, mt }
                         _ -> Nothing
      entrance = case find ((eq Entrance) <<< _.a) indexedMap of
        Nothing -> V { x:100, y:1000 } -- no entrance marker in template; just add one wherever
        Just {x,y} -> V { x, y }
      furniture = placeFurniture indexedMap
      items = placeItems indexedMap
      chart = mkChart (Wall Cave) tiles
  generators <- traverse (\_ -> branch) protoExits
  let exits cid = zipWith (mkExit cid) protoExits (( \rng -> {rng, region}) <$> generators )
      mobs chartId = protoMobs # map \{localPosition, mt } ->
                       mkMob mt (Position { localPosition, chartId})
  pure $ { chart, exits, entrance, furniture, items, mobs }

placeFurniture :: IxArray MapToken -> ChartId -> Map Position Furniture
placeFurniture mapTokens chartId = Map.fromFoldable $ catMaybes
  $ mapTokens # map \{x,y,a} -> do
    c <- getChar a
    fType <- Map.lookup c furnitureByChar
    pure $ Tuple (Position { chartId, localPosition: V{x,y} }) (mkFurniture fType)

placeItems :: IxArray MapToken -> ChartId -> Map Position Item
placeItems mapTokens chartId = Map.fromFoldable $ catMaybes
  $ mapTokens # map \{x,y,a} -> do
    c <- getChar a
    iType <- Map.lookup c itemByChar
    pure $ Tuple (Position { chartId, localPosition: V{x,y} }) (mkItem iType)

rotateLeft :: forall a. Array (Array a) -> Array (Array a)
rotateLeft xs = case sequence $ map unsnoc xs of
  Just pairs -> cons (map (_.last) pairs) (rotateLeft (map (_.init) pairs))
  Nothing -> mempty

rotate :: forall a. Direction -> Array (Array a) -> Array (Array a)
rotate d = repeatedly (Dir.toInt d) rotateLeft

data MapToken = T Tile | Exit Direction | Entrance | Character Char | Monster MobType
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

toMapTokens :: Region -> Array String -> Array (Array (Random MapToken))
toMapTokens r rows = (map (getMapToken r) <<< toCharArray) <$> rows

getMapToken :: Region -> Char -> Random MapToken
getMapToken r '#' = pure $ T $ Wall r
getMapToken _ '^' = pure $ Exit U
getMapToken _ 'v' = pure $ Exit D
getMapToken _ '<' = pure $ Exit L
getMapToken _ '>' = pure $ Exit R
getMapToken _ '!' = pure $ Entrance
getMapToken r '.' = pure $ T $ Floor r
getMapToken r '9' = wallChance r 90
getMapToken r '8' = wallChance r 80
getMapToken r '7' = wallChance r 70
getMapToken r '6' = wallChance r 60
getMapToken r '5' = wallChance r 50
getMapToken r '4' = wallChance r 40
getMapToken r '3' = wallChance r 30
getMapToken r '2' = wallChance r 20
getMapToken r '1' = wallChance r 10
getMapToken _ '?' = Monster <$> element mobs
getMapToken _  c  = pure $ Character c


wallChance :: Region -> Int -> Random MapToken
wallChance r i = do
  isWall <- chance i
  if isWall
    then pure $ T $ Wall r
    else pure $ T $ Floor r
