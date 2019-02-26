module UserInterface where

import Extra.Prelude

import Types (GameState, UIRenderData (..))
import Intent (Action (..))
import Direction (Direction (..))

-- Javascript key codes here: https://keycode.info/

type Key = String

data UI = AwaitingInput UIAwaitingInput | GameAction UIAction
type UIAwaitingInput = { uiRender :: UIRenderData, next :: Key -> UI }
type UIAction = { uiAction :: Action, next :: GameState -> UI }

getGameAction :: UI -> Maybe UIAction
getGameAction (AwaitingInput _) = Nothing
getGameAction (GameAction a) = Just a

getAwaitingInput :: UI -> Maybe UIAwaitingInput
getAwaitingInput (AwaitingInput k) = Just k
getAwaitingInput (GameAction _) = Nothing

uiInit :: GameState -> UIAwaitingInput
uiInit gs = { uiRender: StartScreen, next: const (move gs) }

move :: GameState -> UI
move gs = AwaitingInput { uiRender: MainGame, next }
  where
    next "ArrowLeft"  = GameAction { uiAction: (Move Left), next: move }
    next "ArrowRight" = GameAction { uiAction: (Move Right), next: move }
    next "ArrowDown"  = GameAction { uiAction: (Move Down), next: move }
    next "ArrowUp"    = GameAction { uiAction: (Move Up), next: move }
    next "KeyI"       = inventory gs
    next _            = move gs

inventory :: GameState -> UI
inventory gs = AwaitingInput { uiRender: MainGame, next }
  where
    next "Escape" = move gs
    next _ = inventory gs
