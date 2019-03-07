module Combat where

import Prelude

import Atlas (Position)
import Data.Map (delete)
import Data.Mob (Mob)
import Types (GameState)

data CombatEntity = Player | Monster Mob

doAttack :: GameState -> CombatEntity -> Position -> GameState
doAttack gs attacker pos = case pos == gs.player of
                                      true -> gs
                                      false -> gs {mobs = delete pos gs.mobs} -- Silly one hit kills for the moment.
