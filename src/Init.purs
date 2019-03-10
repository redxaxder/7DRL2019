module Init where

import Extra.Prelude

import Atlas (Position(..), ChartId(..))
import Data.Mob (mobs)
import Map.Gen (initMap)
import Random (newGen)
import Types (GameState, Mob)
import Types.Customer (customerStateFromGen)
import Types.Mob (mkMob, position)

import Data.Array.NonEmpty (head)
import Data.Map (Map)

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
       , items: mempty --exampleItems
       , logevents: mempty
       , mobs: mempty -- exampleMobs
       , placeholders
       , player
       }

exampleMobs :: Map Position Mob
exampleMobs = keyBy position [mob]
  where
  mob = mkMob (head mobs) p
  p = Position {chartId: ChartId 0, localPosition: V {x: 2, y: 5}}
