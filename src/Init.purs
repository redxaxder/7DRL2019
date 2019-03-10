module Init where

import Extra.Prelude

import Atlas (Position(..), ChartId(..))
import Data.Mob (mobs)
import Map.Gen (initMap)
import Random (newGen)
import Types (GameState, Mob, customerStateFromGen)
import Types.Item (Item)
import Types.Mob (mkMob, position)

import Data.Array.NonEmpty (head)
import Data.Map (Map, fromFoldable)

init :: Effect GameState
init = do
  mapGen <- newGen
  customerGen <- newGen
  let { atlas, player, placeholders, furniture } = initMap mapGen
  pure { atlas
       , customerState: customerStateFromGen customerGen
       , distanceMap: mempty
       , fov: mempty
       , furniture
       , inventory: mempty
      --  , inventory: exampleInventory
       , items: mempty --exampleItems
       , logevents: mempty
       , mobs: exampleMobs
       , placeholders
       , player
       }

-- exampleInventory :: Map Char Item
-- exampleInventory = todo fromFoldable
--   [ 'a' |> { name: "Apple" }
--   , 'b' |> { name: "Banapple" }
--   , 'c' |> { name: "Crabapple" }
--   , 'd' |> { name: "Dapple" }
--   ]

exampleMobs :: Map Position Mob
exampleMobs = keyBy position [mob]
  where
  mob = mkMob (head mobs) p
  p = Position {chartId: ChartId 0, localPosition: V {x: 2, y: 5}}
