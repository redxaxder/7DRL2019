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


type Key = String

-- Monoid action on the renderer.
-- Each constructor represents a function `Renderer -> Renderer`
-- Use this to add an inventory overlay, for example.
data UIRenderData = UIRenderData

data UI = AwaitingInput UIRenderData (Key -> GameState -> UI) | GameAction Action UI

uiInit :: UI
uiInit = move

move :: UI
move = AwaitingInput UIRenderData go
  where
    go "ArrowLeft"  _ = GameAction (Move Left) move
    go "ArrowRight" _ = GameAction (Move Right) move
    go "ArrowDown"  _ = GameAction (Move Down) move
    go "ArrowUp"    _ = GameAction (Move Up) move
    go _ _            = move
