module Main where

import Extra.Prelude

import Atlas (getElement, move)
import Control.Monad.Rec.Class (tailRec, Step(..))
import Data.Enum (enumFromTo)
import Data.Map (delete)
import Data.Map as Map
import Data.Set as Set 
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (create, subscribe, sampleOn)
import FRP.Event.Keyboard (down)
import Graphics.Draw (draw)
import Graphics.Render (initCanvas)
import Init (init)
import Intent (Action(..))
import Partial.Unsafe (unsafePartial)
import Tile (blocksMovement)
import Types (GameState)
import UserInterface (Key, UI(..), UIAwaitingInput, uiInit)

main :: Effect Unit
main = unsafePartial $ launchAff_ $ do
  -- initialize canvas
  Just ctx <- initCanvas { canvasId: "game", spritesheetPath: "curses_square_16x16.bmp" }
  { event: engineState, push: pushEngineState } <- liftEffect create
  -- redraw screen in response to state changes
  cancelDraw <- liftEffect $ subscribe engineState $
    \{uia: {uiRender, next}, gs} -> draw ctx uiRender gs
  -- step the game in response to user actions
  cancelEngine <- liftEffect $
    subscribe (sampleOn engineState (stepEngine <$> down)) pushEngineState
  gs <- liftEffect $ init
  liftEffect $ pushEngineState { uia: uiInit gs, gs: gs }

type EngineState = { uia :: UIAwaitingInput, gs :: GameState }

stepEngine :: Key -> EngineState -> EngineState
stepEngine key { uia: {uiRender, next}, gs: g} = tailRec go { ui: (next key), gs: g }
  where
  go :: { ui :: UI, gs :: GameState } -> Step { ui :: UI, gs :: GameState } EngineState
  go { ui, gs } =
    case ui of
         AwaitingInput uia -> Done { uia, gs }
         GameAction { uiAction, next: cont } ->
           case update gs uiAction of
                Nothing -> Loop { ui: cont gs, gs }
                Just nextGs -> Loop { ui: cont nextGs, gs: nextGs }

update :: GameState -> Action -> Maybe GameState
update gs a = pickUpItem <$> handleAction gs a

handleAction :: GameState -> Action -> Maybe GameState
handleAction gs (Move dir) =
  let player = move dir gs.atlas gs.player
      atlas = gs.atlas
   in if blocksMovement (getElement player atlas)
        then Nothing
        else Just $ gs { player = player, atlas = atlas }
handleAction gs (Drop itemChar) =
  let inventory = delete itemChar gs.inventory
  in Just $ gs { inventory = inventory }

letters :: Set.Set Char
letters = Set.fromFoldable (enumFromTo 'a' 'z' :: Array Char)

getInventorySlot :: GameState -> Maybe Char
getInventorySlot gs = if Map.size gs.inventory < 26
                    then Set.findMin $ Set.difference letters (Map.keys gs.inventory)
                    else Nothing

pickUpItem :: GameState -> GameState
pickUpItem gs =
  let maybeUpdated = do
        i <- Map.lookup gs.player gs.items
        c <- getInventorySlot gs
        pure { newInventory: Map.insert c i gs.inventory, newItems: Map.delete gs.player gs.items }
    in case maybeUpdated of
         Nothing -> gs
         Just { newInventory, newItems } -> gs { inventory = newInventory, items = newItems }
