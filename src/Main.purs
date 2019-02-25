module Main where

import Extra.Prelude

import Data.Function (applyFlipped)
import Data.Compactable (compact)
import FRP.Behavior (sampleBy, step, ABehavior, sample)
import FRP.Event (create, subscribe, Event)
import FRP.Event.Keyboard (down)

import Constants (canvasDimensions, font)
import Draw (draw)
import Tile (blocksMovement)
import Atlas (getElement, move, updateAtlas)
import Graphics.Canvas (getCanvasElementById, getContext2D, setCanvasDimensions, setFont)
import Partial.Unsafe (unsafePartial)
import Types (GameState)
import Init (init)
import Intent (Action (..), getAction)
import UserInterface (uiInit, UI(..), Key)


main :: Effect Unit
main = unsafePartial $ do
  Just canvas <- getCanvasElementById "game"
  setCanvasDimensions canvas canvasDimensions
  ctx <- getContext2D canvas
  setFont ctx font
  draw ctx init
  { event: gameStateEvent, push: pushGameState } <- create
  { event: action, push: pushAction } <- create
  { event: uiEvent, push: pushUI } <- create
  let gameState = step init gameStateEvent
      uiState   = step uiInit uiEvent
  void $ subscribe gameStateEvent (draw ctx)
  void $ subscribe (compact $ sampleBy getAction gameState down) pushAction
  void $ subscribe (compact $ sampleBy update gameState action) pushGameState

foo :: ABehavior Event GameState -> ABehavior Event UI -> Event Key -> Event UI
foo gs ui k = sample (updateUI <$> gs <*> ui) (applyFlipped <$> k)

updateUI :: GameState -> UI  -> Key -> UI
updateUI gs key = todo

update :: GameState -> Action -> Maybe GameState
update gs (Move dir) =
  let player = move dir gs.atlas gs.player
      atlas = updateAtlas player gs.atlas
   in if blocksMovement (getElement player atlas)
        then Nothing
        else Just $ gs { player = player, atlas = atlas }
