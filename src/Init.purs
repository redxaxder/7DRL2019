module Init where

import Extra.Prelude

import Data.Map (Map, fromFoldable)
import Types (GameState, Item)
import MapGen (initMap)
import Random (newGen)

init :: Effect GameState
init = do
  gen <- newGen
  let { atlas, player } = initMap gen
  pure { atlas
       , player
       , inventory: exampleInventory
       }

exampleInventory :: Map Char Item
exampleInventory = fromFoldable
  [ 'a' |> { name: "Apple" }
  , 'b' |> { name: "Banapple" }
  , 'c' |> { name: "Crabapple" }
  , 'd' |> { name: "Dapple" }
  ]