module Combat where

import Data.Mob (Mob)
import Extra.Prelude (todo)
import Types (GameState)

data CombatEntity = Player | Monster Mob

doAttack :: GameState -> CombatEntity -> CombatEntity -> GameState
doAttack gs attacker defender = todo
