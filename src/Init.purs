module Init where

import Extra.Prelude

import Atlas (Position(..), ChartId(..))
import Data.Map (Map, fromFoldable)
import Types (GameState, Item)
import Map.Gen (initMap)
import Random (newGen)

init :: Effect GameState
init = do
  gen <- newGen
  let { atlas, player, placeholders } = initMap gen
  pure { atlas
       , player
       , inventory: exampleInventory
       , items: exampleItems
       , placeholders
       , fov: mempty
       }

exampleInventory :: Map Char Item
exampleInventory = fromFoldable
  [ 'a' |> { name: "Apple" }
  , 'b' |> { name: "Banapple" }
  , 'c' |> { name: "Crabapple" }
  , 'd' |> { name: "Dapple" }
  ]

exampleItems :: Map Position Item
exampleItems = fromFoldable
  [  Position {chartId: ChartId 0, localPosition: V {x: 1, y: 1}} |> { name: "Zapple" }
  ]
