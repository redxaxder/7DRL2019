module Combat where

import Prelude

import Extra.Prelude

import Atlas (Position)
import Data.Map (delete, lookup, update)
import Data.Mob (Mob(..))
import Types (GameState)

data CombatEntity = Player | Monster Mob

doAttack :: GameState -> CombatEntity -> Position -> GameState
doAttack gs attacker pos = case pos == gs.player of
                              true -> gs
                              false -> case lookup pos gs.mobs of
                                        Nothing -> gs
                                        Just (Mob mob) -> case ((mob.hp)-1) of
                                                      0     -> gs {mobs = delete pos gs.mobs} -- Silly one hit kills for the moment.
                                                      newhp -> gs {mobs = update (\x -> Just $ Mob {name: mob.name, sprite: mob.sprite, hp: newhp}) pos gs.mobs}
                                                      -- _ -> gs
