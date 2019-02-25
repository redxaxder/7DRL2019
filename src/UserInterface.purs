module UserInterface
  ( uiInit
  , UIRenderData (..)
  , UI (..)
  , Key
  )
  where

import Types (GameState)
import Intent (Action (..))
import Direction (Direction (..))

-- Javascript key codes here: https://keycode.info/

type Key = String
data UIRenderData = UIRenderData
data UI = AwaitingInput UIRenderData (Key -> GameState -> UI) | GameAction Action UI

uiInit :: UI
uiInit = move

showMove :: UIRenderData
showMove = UIRenderData

move :: UI
move = AwaitingInput showMove go
  where
    go "ArrowLeft"  _ = GameAction (Move Left) move
    go "ArrowRight" _ = GameAction (Move Right) move
    go "ArrowDown"  _ = GameAction (Move Down) move
    go "ArrowUp"    _ = GameAction (Move Up) move
    go "KeyI"       _ = inventory
    go _            _ = move

showInventory :: UIRenderData
showInventory = UIRenderData

inventory :: UI
inventory = AwaitingInput showInventory go
  where
    go _ _ = todo
