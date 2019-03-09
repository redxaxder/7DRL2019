module Init where

import Extra.Prelude

import Atlas (Position(..), ChartId(..))
import Data.Array (head)
import Data.Map (Map, fromFoldable)
import Data.Mob (mobs)
import Map.Gen (initMap)
import Random (newGen)
import Types (GameState, Mob)
import Types.Mob (mkMob)

init :: Effect GameState
init = do
  gen <- newGen
  let { atlas, player, placeholders, furniture } = initMap gen
  pure { atlas
       , fov: mempty
       , furniture
       , inventory: mempty --exampleInventory
       , items: mempty --exampleItems
       , placeholders
       , mobs: exampleMobs
       , player
       , logevents: mempty
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
-}
exampleMobs :: Map Position Mob
exampleMobs = fromFoldable
  [ Position {chartId: ChartId 0, localPosition: V {x: 1, y: 2}} |> 
      mkMob (unsafeFromJust $ head mobs)
  , Position {chartId: ChartId 0, localPosition: V {x: 3, y: 2}} |> 
      mkMob (unsafeFromJust $ head mobs)
  ]
--}
