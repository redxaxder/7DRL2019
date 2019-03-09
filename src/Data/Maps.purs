module Data.Maps where

import Extra.Prelude

import Data.Map (Map)
import Data.Map as Map
import Data.Array (filter)
import Data.Array.NonEmpty (fromArray, NonEmptyArray)

import Data.Region (Region (..), regions)

newtype MapData = MapData
  { terrain :: Array String
  , regions :: Array Region
  }

derive instance newtypeMapData :: Newtype MapData _

getTerrain :: MapData -> Array String
getTerrain = _.terrain <<< un MapData

rooms :: Array MapData
rooms = [ sampleMap1, sampleMap2, sampleHall1, sampleHall2, staircase, hardMode ]

roomsByRegion :: Map Region (NonEmptyArray MapData)
roomsByRegion = Map.fromFoldable do
  region <- regions
  let regionRooms = fromArray $ filter (\x -> elem region (un MapData x).regions) rooms
  case regionRooms of
       Nothing -> mempty
       Just rs -> pure (Tuple region rs)

m :: Array Region -> Array String -> MapData
m regions terrain = MapData {terrain, regions}

startRoom :: MapData
startRoom = m mempty
  [ "########^#"
  , "#......#.#"
  , "#....#...#"
  , "#........#"
  , "#...#....#"
  , "#CSOB....#"
  , "##########"
  ]

sampleMap1 :: MapData
sampleMap1 = m [Cave]
  [ "######^######"
  , "#ss.#...#####"
  , "###.......###"
  , "#...#.......>"
  , "!...........#"
  , "###.#.....###"
  , "#####...#####"
  , "######v######"
  ]

hardMode :: MapData
hardMode = m [Cave]
  [ "#<.>#"
  , "^#.#^"
  , "....."
  , "v#.#v"
  , "#!.>#"
  ]

sampleMap2 :: MapData
sampleMap2 = m [Cave]
  [ "############^"
  , "#............"
  , "#...........#"
  , "#...........#"
  , "#...........#"
  , "#...........>"
  , "!.###########"
  ]

sampleHall1 :: MapData
sampleHall1 = m [Cave]
  [ "#######"
  , "!.....>"
  , "#######"
  ]

sampleHall2 :: MapData
sampleHall2 = m [Cave]
  [ "###^######"
  , "!........#"
  , "#######v##"
  ]

staircase :: MapData
staircase = m [Cave]
  [ "###"
  , "<.#"
  , "#.#"
  , "#!#"
  ]
