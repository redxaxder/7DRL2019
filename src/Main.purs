module Main where

import Extra.Prelude

import Control.Monad.Rec.Class (tailRec, Step(..))
import Control.Monad.State (execState, State, get, modify_, modify)
import Data.Array (cons, filter, find)
import Data.Array as Array
import Data.Map (delete, toUnfoldable, lookup)
import Data.Map as Map
import Data.Maybe (isJust)
import Data.Set as Set
import Data.Symbol (SProxy(..))
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import FOV (visibleTiles)
import FRP.Event (create, subscribe, sampleOn)
import FRP.Event.Keyboard (down)
import Map.Gen (expandMap)
import Combat (doAttack)
import Data.Enum (enumFromTo)
import Combat (doAttack, attackPlayer)
import Data.Tile (blocksMovement, Tile(..))
import Data.Tuple (fst, snd)
import Direction (Direction(..))
import Types (GameState, LogEvent(..), Mob, FieldOfView, liftMobState)
import Types.Item (itemName)
import Types.Mob (Mob, position, moveMob)
import Graphics.Draw (draw)
import Graphics.Render (initCanvas)
import Init (init)
import Intent (Action(..))
import Partial.Unsafe (unsafePartial)
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
stepEnvironment = doMobStuff <<< pickUpItem <<< tailRec expand
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
         Nothing -> gs
         Just { newInventory, newItems, acquiredItem } -> gs { inventory = newInventory, items = newItems, logevents = cons (ItemEvent acquiredItem) gs.logevents }

findAdjacent :: (Position -> Direction -> Boolean)
                -> Position
                -> Maybe Direction
findAdjacent f pos = find (f pos) [R, L, U, D]

passable :: GameState -> Position -> Boolean
passable gs pos = not ((blocksMovement $ getElement pos gs.atlas) || (pos == gs.player) || (isJust $ lookup pos gs.mobs))
-- Mob -> Tuple (Mob a)
-- liftMobStuff :: State Mob a -> State GameState a
-- liftMobStuff = todo

-- first try R, L, then U, D
-- if player adjacent, attack player
-- if blocksMovement direction, then try next direction
-- if neither, move into space
monsterAction :: GameState -> GameState
monsterAction gs = foldr individualMonsterAction gs (Array.fromFoldable $ Map.values gs.mobs)
  where
    individualMonsterAction :: Mob -> GameState -> GameState
    individualMonsterAction mob gs' = let mobPos = position mob in
      case playerAdjacent mobPos of
        Just dir -> attackPlayer gs' mob
        Nothing -> let 
            findEmptySpace :: Position -> Maybe Direction
            findEmptySpace = findAdjacent findEmptySpaceDirection

            findEmptySpaceDirection :: Position -> Direction -> Boolean
            findEmptySpaceDirection pos dir = let targetPos = move dir gs.atlas pos
              -- in not $ blocksMovement $ getElement targetPos gs.atlas
              in passable gs' targetPos
          in case findEmptySpace mobPos of
            Just moveDir ->
              let gs'' = gs' { mobs = Map.delete mobPos gs'.mobs }
                  targetPos = move moveDir gs'.atlas mobPos
              in gs'' { mobs = Map.insert targetPos mob gs''.mobs }
            Nothing -> gs'
          
    playerAdjacent :: Position -> Maybe Direction
    playerAdjacent = findAdjacent playerAdjacentDirection
    
    playerAdjacentDirection :: Position -> Direction -> Boolean
    playerAdjacentDirection pos dir = (move dir gs.atlas pos) == gs.player





doMobStuff :: GameState -> GameState
doMobStuff gs = flip execState gs $ (Map.keys gs.mobs) # traverse_ \mobPos -> do
  maction <- liftMobState mobPos (individualMonsterAction2)
  case spy "a" maction of
    Nothing -> pure unit
    Just action -> modify_ $ interpretMobAction action


interpretMobAction :: MobAction -> GameState -> GameState
interpretMobAction (MobAttack mob) gs = attackPlayer gs mob
interpretMobAction (MobPass mob) gs = gs

data MobAction = MobAttack Mob | MobPass Mob
individualMonsterAction2 :: GameState -> State Mob MobAction
individualMonsterAction2 gs = do
  mob <- get
  let mobPos = position mob
  case playerAdjacent mobPos of
    Just dir -> MobAttack <$> get
    Nothing -> case findEmptySpace mobPos of
      Just moveDir -> do
        modify_ $ moveMob moveDir gs.atlas
        MobPass <$> get
      Nothing -> MobPass <$> get
  where
    playerAdjacent :: Position -> Maybe Direction
    playerAdjacent = findAdjacent playerAdjacentDirection
    
    playerAdjacentDirection :: Position -> Direction -> Boolean
    playerAdjacentDirection pos dir = (move dir gs.atlas pos) == gs.player

    findEmptySpace :: Position -> Maybe Direction
    findEmptySpace = findAdjacent findEmptySpaceDirection

    findEmptySpaceDirection :: Position -> Direction -> Boolean
    findEmptySpaceDirection pos dir = let targetPos = move dir gs.atlas pos
      in not $ blocksMovement $ getElement targetPos gs.atlas

  