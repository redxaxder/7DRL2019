module UserInterface where

import Extra.Prelude

import Types (GameState)
import Intent (Action (..))
import Direction (Direction (..))


type Key = String

data UIRenderData = UIRenderData

data UI = AwaitingInput UIRenderData (Key -> GameState -> UI) | GameAction Action UI

readyToMove :: UI
readyToMove = AwaitingInput UIState move
  where
    move "ArrowLeft"  _ = GameAction (Move Left) readyToMove
    move "ArrowRight" _ = GameAction (Move Right) readyToMove
    move "ArrowDown"  _ = GameAction (Move Down) readyToMove
    move "ArrowUp"    _ = GameAction (Move Up) readyToMove
    move _ _            = readyToMove
