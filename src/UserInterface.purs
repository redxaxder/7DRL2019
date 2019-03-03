module UserInterface where

import Extra.Prelude

import Types (GameState, UIRenderData (..), Item)
import Intent (Action (..))
import Direction (Direction (..))
import Data.String.CodePoints (stripPrefix)
import Data.String.CodeUnits (toChar)
import Data.String (toLower)
import Data.String.Pattern (Pattern (..))

import Data.Map (lookup)
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
    next "ArrowLeft"  = GameAction { uiAction: (Move L), next: move }
    next "ArrowRight" = GameAction { uiAction: (Move R), next: move }
    next "ArrowDown"  = GameAction { uiAction: (Move D), next: move }
    next "ArrowUp"    = GameAction { uiAction: (Move U), next: move }
    next "KeyI"       = inventory gs
    next _            = move gs

getCharacter :: String -> Maybe Char
getCharacter s = stripPrefix (Pattern "key") (toLower s) >>= toChar

getDigit :: String -> Maybe Char
getDigit s = stripPrefix (Pattern "Digit") s >>= toChar


inventory :: GameState -> UI
inventory gs = AwaitingInput { uiRender: InventoryScreen Nothing, next }
  where
    next key = case getCharacter key of
      Nothing -> move gs -- it's not a letter; exit back to main UI
      Just d -> -- it's a letter; enter the subinventory screen for the corresponding item (if exists). stay here (if doesn't exist)
        case (lookup d gs.inventory) of
          Nothing -> inventory gs
          Just selectedItem -> subInventory gs d selectedItem

subInventory :: GameState -> Char -> Item -> UI
subInventory gs label item = AwaitingInput { uiRender: InventoryScreen (Just {label, item}), next }
  where
    next "KeyD" = GameAction { uiAction: Drop label, next: move }
    next "Escape" = inventory gs
    next _ = subInventory gs label item