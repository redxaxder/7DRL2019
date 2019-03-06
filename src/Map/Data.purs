module Map.Data where

import Extra.Prelude
import Types(MapGenHint, MapData)

rooms :: Array MapData
rooms = [ sampleMap1, sampleMap2, sampleHall1, sampleHall2, staircase ]

startRoom :: MapData
startRoom = 
  { terrain:
      [ "##########"
      , "#......#^#"
      , "#....#...#"
      , "#........#"
      , "#...#....#"
      , "#........#"
      , "##########"
      ]
  }

sampleMap1 :: MapData
sampleMap1 = 
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
  }

sampleMap2 :: MapData
sampleMap2 = 
  {
    terrain:
      [ "############^"
      , "#............"
      , "#...........#"
      , "#...........#"
      , "#...........#"
      , "#...........>"
      , "#!###########"
      ]
  }

sampleHall1 :: MapData
sampleHall1 = 
  { terrain: 
    [ "#######"
    , "!.....>"
    , "#######"
    ]
  }

sampleHall2 :: MapData
sampleHall2 =
 {
   terrain:
   [ "###^######"
   , "!........#"
   , "#######v##" 
   ]
 }

staircase :: MapData  
staircase = 
  { terrain:
    [ "###"
    , "<.#"
    , "#.#"
    , "#!#"
    , "###"
    ]
  }  