module Init where

import Data.Array (head)
import Data.Map (Map, fromFoldable)
import Data.Sprite (spriteAt)
import Extra.Prelude

import Atlas (Position(..), ChartId(..))
import Data.Mob (Mob(..), MobName(..), mobs)
import Map.Gen (initMap)
import Random (newGen)
import Types (GameState, Item)

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
       -- , mobs: mempty --exampleMobs
       , mobs: exampleMobs
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
  [ Position {chartId: ChartId 0, localPosition: V {x: 2, y: 2}} |> case head mobs of
                                                                        Just a -> a
                                                                        Nothing -> Mob { name: MobName "Broken", sprite: spriteAt 4 3, hp: 1 }
  ]
--}
