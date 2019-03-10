module UserInterface where

import Extra.Prelude

import Control.MonadZero (guard)
import Data.Array (cons, delete, filter)
import Data.Array as Array
import Data.Foldable (find, all)
import Data.Map as Map
import Data.String.CodePoints (stripPrefix)
import Data.String.CodeUnits (toChar)
import Data.String (toLower)
import Data.String.Pattern (Pattern (..))

import Atlas (move)
import Data.Recipe (getRecipes, recipeCanUse)
import Types (GameState, UIRenderData (..), Item, UIHint(..), Action(..), canServe)
import Types.Item (itemType)
import Direction (Direction (..))
import Types.Furniture (counter, furnitureType)

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

mainUIHints :: Array UIHint
mainUIHints =
  [ UIHint "KeyC" "Crafting"
  , UIHint "KeyI" "Inventory"
  , UIHint "Period" "Pass"
  ]

main :: GameState -> UI
main gs = AwaitingInput { uiRender: MainGame mainUIHints, next }
  where
    next "ArrowLeft"  = moveOrCraft gs L
    next "KeyH"       = moveOrCraft gs L
    next "ArrowRight" = moveOrCraft gs R
    next "KeyL"       = moveOrCraft gs R
    next "ArrowDown"  = moveOrCraft gs D
    next "KeyJ"       = moveOrCraft gs D
    next "ArrowUp"    = moveOrCraft gs U
    next "KeyK"       = moveOrCraft gs U
    next "KeyI"       = inventory gs
    next "Space"      = GameAction { uiAction: Pass, next: main }
    next "Period"     = GameAction { uiAction: Pass, next: main }
    next "KeyC"       = crafting gs mempty
    next _            = main gs

moveOrCraft :: GameState -> Direction -> UI
moveOrCraft gs d =
  let targetPos = move d gs.atlas gs.player
   in case Map.lookup targetPos gs.furniture of
           Nothing -> GameAction { uiAction: (Move d), next: main }
           Just furniture -> if furnitureType furniture == counter
                               then chooseItemToServe gs
                               else crafting gs mempty

chooseItemHints :: Array UIHint
chooseItemHints =
  [ UIHint "Escape" "Back"
  ]

chooseItemToServe :: GameState -> UI
chooseItemToServe gs = AwaitingInput { uiRender, next }
  where
    uiRender = ServeCustomerScreen chooseItemHints

    next "Escape" = main gs
    next key = fromMaybe (chooseItemToServe gs) do
      c <- getCharacter key
      item <- lookup c gs.inventory
      guard (canServe item gs)
      pure $ GameAction { uiAction: Serve c, next: main }

craftingUIHints :: Array UIHint
craftingUIHints =
  [ UIHint "Escape" "Back"
  , UIHint "letter" "Toggle item"
  , UIHint "enter" "Craft"
  ]

crafting :: GameState -> Array { label :: Char, item :: Item } -> UI
crafting gs selected = AwaitingInput { uiRender: Crafting selected shownRecipes craftingUIHints, next }
  where
    shownRecipes = getRecipes (Array.fromFoldable $ itemType <$> Map.values gs.inventory)
       # filter \r -> all (recipeCanUse r) (itemType <<< _.item <$> selected)
    next "Escape" = main gs
    next "Enter" = main gs
    next "Space" = main gs -- TODO: craft the item!
    next key = fromMaybe (crafting gs selected) $ do
      label <- getCharacter key
      item <- Map.lookup label gs.inventory
      let selected' = case find (\x -> x.label == label) selected of
                        Nothing -> cons { label, item } selected
                        Just x -> delete x selected
      pure $ crafting gs selected'

inventoryUIHints :: Array UIHint
inventoryUIHints =
  [ UIHint "letter" "Select item"
  , UIHint "Escape" "Back"
  ]

inventory :: GameState -> UI
inventory gs = AwaitingInput { uiRender: InventoryScreen Nothing inventoryUIHints, next }
  where
    next "Escape" = main gs
    next key = fromMaybe (inventory gs) do
      c <- getCharacter key
      selectedItem <- lookup c gs.inventory
      pure $ subInventory gs c selectedItem

subInventoryUIHints :: Array UIHint
subInventoryUIHints =
  [ UIHint "KeyD" "Drop"
  , UIHint "Escape" "Deselect"
  ]

subInventory :: GameState -> Char -> Item -> UI
subInventory gs label item = AwaitingInput { uiRender: InventoryScreen (Just {label, item}) subInventoryUIHints, next }
  where
    next "KeyD" = GameAction { uiAction: Drop label, next: main }
    next "Escape" = inventory gs
    next _ = subInventory gs label item

getCharacter :: String -> Maybe Char
getCharacter s = stripPrefix (Pattern "key") (toLower s) >>= toChar

getDigit :: String -> Maybe Char
getDigit s = stripPrefix (Pattern "Digit") s >>= toChar
