module UserInterface where

import Extra.Prelude

import Control.MonadZero (guard)
import Data.Array (cons, delete, filter)
import Data.Array as Array
import Data.Map as Map
import Data.String.CodePoints (stripPrefix)
import Data.String.CodeUnits (toChar)
import Data.String (toLower)
import Data.String.Pattern (Pattern (..))

import Atlas (move)
import Data.Recipe (getRecipes, recipeCanUse, canCraft)
import Direction (Direction (..))
import Types (GameState, UIRenderData (..), Item, UIHint(..), Action(..), canServe)
import Types.Item (itemType)
import Types.Furniture (FurnitureType, counter, furnitureType)

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
    next "KeyC"       = crafting gs Nothing mempty
    next _            = main gs

moveOrCraft :: GameState -> Direction -> UI
moveOrCraft gs d =
  let targetPos = move d gs.atlas gs.player
   in case Map.lookup targetPos gs.furniture of
        Nothing -> GameAction { uiAction: (Move d), next: main }
        Just furniture ->
          if furnitureType furniture == counter
            then chooseItemToServe gs
            else crafting gs (Just $ furnitureType furniture) mempty

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
  [ UIHint "letter" "Toggle item"
  , UIHint "Enter" "Craft"
  , UIHint "Escape" "Back"
  ]

crafting ::
  GameState ->
  Maybe FurnitureType ->
  Array { label :: Char, item :: Item } ->
  UI
crafting gs furniture selected = AwaitingInput { uiRender, next }
  where
    uiRender = Crafting selected shownRecipes craftingUIHints furniture
    shownRecipes = getRecipes (Array.fromFoldable $ itemType <$> Map.values gs.inventory)
       # filter (\r -> all (recipeCanUse r) (itemType <<< _.item <$> selected))
       # filter (canCraft furniture)
    next "Escape" = main gs
    next "Enter" = main gs
    next "Space" = main gs -- TODO: craft the item!
    next key = fromMaybe (crafting gs furniture selected) $ do
      label <- getCharacter key
      item <- Map.lookup label gs.inventory
      let selected' = case find (\x -> x.label == label) selected of
                        Nothing -> cons { label, item } selected
                        Just x -> delete x selected
      pure $ crafting gs furniture selected'

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
  , UIHint "Escape" "Back"
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
