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
rooms = [ sampleMap1, sampleMap2, sampleMap3, sampleHall1, sampleHall2, staircase, big, hardMode ]

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
  , "##B.S.O#.#"
  , "##.......#"
  , "##.......#"
  , "##.......#"
  , "####C#####"
  , "#........#"
  , "#........#"
  , "#........#"
  , "#........#"
  , "#........#"
  , "#........#"
  , "#........#"
  , "#........#"
  , "#........#"
  , "#........#"
  , "#........#"
  , "#........#"
  , "#........#"
  , "#........#"
  , "##########"
  ]

sampleMap1 :: MapData
sampleMap1 = m [Cave]
  [ "#####^#####"
  , "#ss#?..####"
  , "##5#3.....>"
  , "!.........#"
  , "####....###"
  , "#####v#####"
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
  [ "##########^"
  , "#.....?...."
  , "#.........#"
  , "#.......s.#"
  , "#.........#"
  , "#.....?...>"
  , "!.#########"
  ]

sampleMap3 :: MapData
sampleMap3 = m [Cave]
  [ "###222##6666......5#"
  , "<8.1222......7###..4"
  , "##.#1##.###322#^##.4"
  , "!..#1##.###3###3##.4"
  , "##.#....###3###....4"
  , "11.#.###1113#...#111"
  , "11...6?###....#3#1##"
  , "#8##6###...55##34?#^"
  , "#.?#.....###5.?#4111"
  , "#....33...111###4##4"
  , "###7##..####22?21111"
  , "#<7.55.11##1112211##"
  , "##1.11.51111###2214>"
  , "##8##..71##11111#1##"
  , "#?.6..#?##3?1####?##"
  , "###3.##v###1177##11#"
  , "##5...####61151>##11"
  , "##v##.####71#8###111"
  , "####2.##<4.....11?1#"
  , "#^###.####..#4.##1##"
  , "#2?66......###v##44>"
  ]

sampleHall1 :: MapData
sampleHall1 = m [Cave]
  [ "####"
  , "!..>"
  , "####"
  ]

big :: MapData
big = m [Cave]
  [ "###22#^#2#2#2?222###"
  , "##.....2222#22222#^#"
  , "#2.2222#2.......2444"
  , "22........22222.2222"
  , "###22222222...2.222#"
  , "#2222222....2...22##"
  , "222222...2222.22?22#"
  , "##7222.22##22222####"
  , "#?7....22222222#####"
  , "###.2222222222552>##"
  , "222.22#222222#######"
  , "#2.....22#222222222#"
  , "222222.222......2###"
  , "#v###2.....##22.222#"
  , "#####2222222222....#"
  , "##^##2222222277722.#"
  , "2#6#22222222##?##2.2"
  , "222222222222####?2.2"
  , "##222222222222..2..#"
  , "2222222222........22"
  , "22?222#222.2##222###"
  , "222222222..2222#22##"
  , "#222222...222##<722#"
  , "!.......222222###222"
  , "####22###22#2222>###"
  ]
sampleHall2 :: MapData
sampleHall2 = m [Cave]
  [ "#^######"
  , "!....7?#"
  , "####v###"
  ]

staircase :: MapData
staircase = m [Cave]
  [ "###"
  , "<.#"
  , "#.#"
  , "#!#"
  ]
