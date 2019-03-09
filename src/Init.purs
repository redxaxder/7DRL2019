module Init where

import Extra.Prelude

import Atlas (Position(..), ChartId(..))
import Data.Array (head)
import Data.Map (Map)
import Data.Mob (mobs)
import Map.Gen (initMap)
import Random (newGen)
import Types (GameState, Mob, customerStateFromGen)
import Types.Mob (mkMob, position)

init :: Effect GameState
init = do
  mapGen <- newGen
  customerGen <- newGen
  let { atlas, player, placeholders, furniture } = initMap mapGen
  pure { atlas
       , fov: mempty
       , furniture
       , inventory: mempty --exampleInventory
       , items: mempty --exampleItems
       , placeholders
       , mobs: exampleMobs
       , player
       , logevents: mempty
       , customerState: customerStateFromGen customerGen
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
exampleMobs = keyBy position [mob]
  where 
  mob = mkMob (unsafeFromJust $ head mobs) p
  p = Position {chartId: ChartId 0, localPosition: V {x: 2, y: 2}}

--}
