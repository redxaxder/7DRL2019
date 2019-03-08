module Main where

import Extra.Prelude

import Data.Enum (enumFromTo)
import Data.Map (delete)
import Data.Map as Map
import Data.Set as Set
import Control.Monad.State (execState, State, get, modify_)
import Control.Monad.Rec.Class (tailRec, Step(..))
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FRP.Event (create, subscribe, sampleOn)
import FRP.Event.Keyboard (down)
import Partial.Unsafe (unsafePartial)

import Atlas (getElement, move, Position)
import Combat (doAttack)
import Data.Tile (blocksMovement)
import FOV (visibleTiles)
import Graphics.Draw (draw)
import Graphics.Render (initCanvas)
import Init (init)
import Intent (Action(..))
import Map.Gen (expandMap)
import Types (GameState, LogEvent(..), Mob)
import Types.Item (itemName)
import UserInterface (Key, UI(..), UIAwaitingInput, uiInit)

main :: Effect Unit
main = unsafePartial $ launchAff_ $ do
  -- initialize canvas
  Just ctx <- initCanvas { canvasId: "game", spritesheetPath: "tileset.png" }
  { event: engineState, push: pushEngineState } <- liftEffect create
  -- redraw screen in response to state changes
  cancelDraw <- liftEffect $ subscribe engineState $
    \{uia: {uiRender, next}, gs} -> draw ctx uiRender gs
  -- step the game in response to user actions
  cancelEngine <- liftEffect $
    subscribe (sampleOn engineState (stepEngine <$> down)) pushEngineState
  gs <- liftEffect $ stepEnvironment <$> init
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
update gs a = stepEnvironment <$> handleAction gs a

stepEnvironment :: GameState -> GameState
stepEnvironment = stepMobs <<< pickUpItem <<< tailRec expand
  where
  expand :: GameState -> Step GameState GameState
  expand gs = let gs' = updateFOV gs
              in case expandMap gs' of
                    Nothing -> Done gs'
                    Just gs'' -> Loop gs''


updateFOV :: GameState -> GameState
updateFOV gs = gs { fov = visibleTiles 10 gs }

handleAction :: GameState -> Action -> Maybe GameState
handleAction gs (Move dir) =
  let player = move dir gs.atlas gs.player
      atlas = gs.atlas
   in case Map.lookup player gs.mobs of
        Nothing -> if blocksMovement (getElement player atlas)
                      then Nothing
                      else Just $ gs { player = player, atlas = atlas }
        Just a -> Just $ doAttack gs player
handleAction gs (Drop itemChar) =
  let inventory = delete itemChar gs.inventory
  in Just $ gs { inventory = inventory }
handleAction gs Pass = Just gs

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
        pure { newInventory: Map.insert c i gs.inventory, newItems: Map.delete gs.player gs.items, acquiredItem: i }
    in case maybeUpdated of
         Nothing -> gs { logevent = Nothing }
         Just { newInventory, newItems, acquiredItem } -> gs { inventory = newInventory, items = newItems, logevent = Just $ ItemEvent acquiredItem }

stepMobs :: GameState -> GameState
stepMobs gs = 
  let mobKeys = Map.keys gs.mobs
  in  flip execState gs $ mobKeys # traverse_ \k -> do
        mmob <- Map.lookup k <<< _.mobs <$> get
        case mmob of
          Nothing -> pure unit
          Just mob -> do
            modify_ $ (\x -> x{ mobs = Map.delete k x.mobs })
            player <- _.player <$> get
            let Tuple k' mob' = execState (getMobAction player (Tuple k mob)) (Tuple k mob)
            modify_ $ (\x -> x{ mobs = Map.insert k' mob' x.mobs })

getMobAction :: Position -> Tuple Position Mob -> MobAction
getMobAction = todo

type MobAction = State (Tuple Position Mob) Unit -- Tuple Position Mob -> Tuple (Tuple Poition Mob) Unit
