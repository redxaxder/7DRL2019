module Combat where

import Extra.Prelude

import Data.Array (cons)
import Data.Map (update, lookup)

import Atlas (Position)
import Types.Mob (hit, Mob)
import Types (GameState, LogEvent(..))

doAttack :: GameState -> Position -> GameState
doAttack gs pos = case lookup pos gs.mobs of
  Just mob -> case hit mob of
    Just mob' -> gs { mobs = update hit pos gs.mobs, logevents = cons (CombatEvent mob') gs.logevents }
    Nothing -> gs { mobs = update hit pos gs.mobs, logevents = cons (MonsterKilledEvent mob) gs.logevents }
  Nothing -> gs

-- just log for now, there is no player hp
attackPlayer :: GameState -> Mob -> GameState
attackPlayer gs mob = gs { logevents = cons (PlayerAttacked mob) gs.logevents }
