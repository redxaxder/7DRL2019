module Combat where

import Extra.Prelude

import Data.Array (cons)
import Data.Map (update, lookup, singleton, member)
import Control.Monad.State (State, modify, execState)

import Atlas (Position)
import Types.Mob (hit, mobDrop)
import Types (GameState, LogEvent(..), Mob)

doAttack :: GameState -> Position -> GameState
doAttack gs pos = case lookup pos gs.mobs of
  Nothing -> gs
  Just mob -> gs # execState do
     alive <- doMobHit pos mob
     if alive then logEvent $ CombatEvent mob
       else do
          logEvent $ MonsterKilledEvent mob
          doItemDrop pos mob

-- hit the mob. return true if it is still alive
doMobHit :: Position -> Mob -> State GameState Boolean
doMobHit p m = do
  gs' <- modify (\gs -> gs { mobs = update hit p gs.mobs } )
  pure $ member p gs'.mobs

logEvent :: LogEvent -> State GameState Unit
logEvent e = void $ modify \gs -> gs { logevents = cons e gs.logevents }

doItemDrop :: Position -> Mob -> State GameState Unit
doItemDrop pos m = case mobDrop m of
  Nothing -> pure unit
  Just d -> void $ modify \gs -> gs { items = singleton pos d <> gs.items }

