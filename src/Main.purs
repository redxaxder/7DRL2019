module Main where

import Extra.Prelude

import Control.Monad.Rec.Class (tailRec, Step (..))
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import FRP.Event (create, subscribe, sampleOn)
import FRP.Event.Keyboard (down)

import Atlas (getElement, move, updateAtlas)
import Constants (canvasDimensions, font)
import Draw (draw)
import Graphics.Canvas (getCanvasElementById, getContext2D, setCanvasDimensions, setFont)
import Init (init)
import Intent (Action (..))
import Partial.Unsafe (unsafePartial)
import Tile (blocksMovement)
import Types (GameState)
import UserInterface (uiInit, UI(..), Key, UIAwaitingInput)
import ResourceLoading (loadResources)


main :: Effect Unit
main = unsafePartial $ launchAff_ $ do
  Just canvas <- liftEffect $ getCanvasElementById "game"
  liftEffect $ setCanvasDimensions canvas canvasDimensions
  ctx <- liftEffect $ getContext2D canvas
  resourceList <- loadResources
  case resourceList of
       Nothing -> log "resource loading failed"
       Just resources -> void $ sequence $ log <$> resources
  liftEffect $ setFont ctx font
  { event: engineState, push: pushEngineState } <- liftEffect create
  -- redraw screen in response to state changes
  cancelDraw <- liftEffect $ subscribe engineState $
    \{uia: {uiRender, next}, gs} -> draw ctx uiRender gs
  -- step the game in response to user actions
  cancelEngine <- liftEffect $
    subscribe (sampleOn engineState (stepEngine <$> down)) pushEngineState
  liftEffect $ pushEngineState { uia: uiInit init, gs: init }

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
update gs (Move dir) =
  let player = move dir gs.atlas gs.player
      atlas = updateAtlas player gs.atlas
   in if blocksMovement (getElement player atlas)
        then Nothing
        else Just $ gs { player = player, atlas = atlas }
