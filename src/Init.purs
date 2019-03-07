module Init where

import Extra.Prelude

import Types (GameState)
import Map.Gen (initMap)
import Random (newGen)
import Types (GameState, Item, Mob)

init :: Effect GameState
init = do
  gen <- newGen
  let { atlas, player, placeholders } = initMap gen
  pure { atlas
       , player
       , inventory: mempty --exampleInventory
       , items: mempty --exampleItems
       , placeholders
       , fov: mempty
       , mobs: mempty --exampleMobs
       }

{-
exampleInventory :: Map Char Item
exampleInventory = todo fromFoldable
  [ 'a' |> { name: "Apple" }
  , 'b' |> { name: "Banapple" }
   'c' |> { name: "Crabapple" }
  , 'd' |> { name: "Dapple" }
  ]

exampleItems :: Map Position Item
exampleItems = fromFoldable
  [  Position {chartId: ChartId 0, localPosition: V {x: 3, y: 1}} |> { name: "Zapple" }
  ]

exampleMobs :: Map Position Mob
exampleMobs = fromFoldable 
  [ Position {chartId: ChartId 0, localPosition: V {x: 2, y: 2}} |> { name: "Bananamatronic Husk", gfx: bananamatronicHusk }

  ]
-}
