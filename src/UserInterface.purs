module UserInterface where

import Extra.Prelude

import Data.Map as Map
import Data.String.CodePoints (stripPrefix)
import Data.String.CodeUnits (toChar)
import Data.String (toLower)
import Data.String.Pattern (Pattern (..))

import Atlas (move)
import Types (GameState, UIRenderData (..), Item)
import Intent (Action (..))
import Direction (Direction (..))
import Data.Furniture (counter, furnitureType)

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
uiInit gs = { uiRender: StartScreen, next: const (main gs) }

main :: GameState -> UI
main gs = AwaitingInput { uiRender: MainGame, next }
  where
    next "ArrowLeft"  = moveOrCraft gs L
    next "ArrowRight" = moveOrCraft gs R
    next "ArrowDown"  = moveOrCraft gs D
    next "ArrowUp"    = moveOrCraft gs U
    next "KeyI"       = inventory gs
    next _            = main gs

moveOrCraft :: GameState -> Direction -> UI
moveOrCraft gs d =
  let targetPos = move d gs.atlas gs.player
   in case Map.lookup targetPos gs.furniture of
           Nothing -> GameAction { uiAction: (Move d), next: main }
           Just furniture -> if furnitureType furniture == counter
                               then inventory gs -- TODO: to item serving
                               else inventory gs -- TODO: to crafting
inventory :: GameState -> UI
inventory gs = AwaitingInput { uiRender: InventoryScreen Nothing, next }
  where
    next key = case getCharacter key of
      Nothing -> main gs -- it's not a letter; exit back to main UI
      Just d -> -- it's a letter; enter the subinventory screen for the corresponding item (if exists). stay here (if doesn't exist)
        case (lookup d gs.inventory) of
          Nothing -> inventory gs
          Just selectedItem -> subInventory gs d selectedItem

subInventory :: GameState -> Char -> Item -> UI
subInventory gs label item = AwaitingInput { uiRender: InventoryScreen (Just {label, item}), next }
  where
    next "KeyD" = GameAction { uiAction: Drop label, next: main }
    next "Escape" = inventory gs
    next _ = subInventory gs label item

getCharacter :: String -> Maybe Char
getCharacter s = stripPrefix (Pattern "key") (toLower s) >>= toChar

getDigit :: String -> Maybe Char
getDigit s = stripPrefix (Pattern "Digit") s >>= toChar
