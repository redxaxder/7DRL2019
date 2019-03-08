module Combat where

import Extra.Prelude

import Data.Map (update)

import Atlas (Position)
import Types.Mob (hit)
import Types (GameState)

doAttack :: GameState -> Position -> GameState
doAttack gs pos = gs { mobs = update hit pos gs.mobs }
