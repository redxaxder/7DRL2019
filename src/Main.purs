module Main where

import Extra.Prelude

import Control.Monad.Rec.Class (tailRec, Step(..))
import Control.Monad.State
  ( execState
  , State
  , get
  , modify_
  , evalState
  )
import Data.Array (cons, filter)
import Data.Array as Array
import Data.Array.NonEmpty (cons', head, sortBy)
import Data.Enum (enumFromTo)
import Data.Map (delete, lookup)
import Data.Map as Map
import Data.Set as Set
import Effect.Aff (launchAff_)
import Effect.Class (liftEffect)
import Partial.Unsafe (unsafePartial)

import Atlas (move, getElement, Position)
import Combat (doAttack, attackPlayer)
import Data.Tile (blocksMovement)
import Data.Recipe (haveMats, mats)
import Direction (Direction(..))
import DistanceMap (makeDistanceMap)
import FOV (visibleTiles)
import FRP.Event (create, subscribe, sampleOn)
import FRP.Event.Keyboard (down)
import Graphics.Draw (draw)
import Graphics.Render (initCanvas)
import Init (init)
import Map.Gen (expandMap)
import Types
  ( GameState
  , LogEvent(..)
  , Mob
  , liftMobState
  , liftCustomerState
  , applyReward'
  , liftInventoryState
  , Action(..)
  )
import Types.Customer (tickCustomers, serveCustomer)
import Types.Item (Item, ItemType, itemType, mkItem)
import Types.Mob (position, moveMob')
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
stepEnvironment = tailRec expand
  >>> pickUpItem
  >>> updateDistanceMap
  >>> doMobStuff
  >>> customers
  where
  expand :: GameState -> Step GameState GameState
  expand gs = let gs' = updateFOV gs
              in case expandMap gs' of
                    Nothing -> Done gs'
                    Just gs'' -> Loop gs''


updateFOV :: GameState -> GameState
updateFOV gs = gs { fov = visibleTiles 10 gs }

updateDistanceMap :: GameState -> GameState
updateDistanceMap gs = gs { distanceMap = makeDistanceMap 10 gs.player gs.atlas }

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

handleAction gs (Serve itemChar) = flip evalState gs do
  mitem <- liftInventoryState $ Map.lookup itemChar <$> get
  case mitem of
    Nothing -> pure Nothing
    Just item -> do
      liftCustomerState (serveCustomer item)
      liftInventoryState $ modify_ $ delete itemChar
      Just <$> get

handleAction gs (Craft recipe) =
  case recipe of
    Nothing -> Nothing
    Just rec ->
      if haveMats gs.inventory rec then -- Check that we have the mats to make this safe
          let
              deleteLeastByItemType :: ItemType -> Map.Map Char Item -> Map.Map Char Item
              deleteLeastByItemType it inv = delete c inv
                where
                  c = fst $ unsafeFromJust $ Array.head charItemArray
                  charItemArray :: Array (Tuple Char Item)
                  charItemArray = filter selectItem $ Map.toUnfoldable inv
                  selectItem (char |> item) = itemType item == it
              inventory' = foldr deleteLeastByItemType gs.inventory $ mats rec
              insertAtKey k = Map.insert k (mkItem rec.output) inventory'
              inventory'' = insertAtKey <$> getInventorySlot gs
            in case inventory'' of
                  Nothing -> Nothing
                  Just a -> Just (gs {inventory = a})
        else Nothing


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
passable gs pos = not ((blocksMovement $ getElement pos gs.atlas)
                    || (pos == gs.player)
                    || (isJust $ lookup pos gs.mobs))

-- monsterAction :: GameState -> GameState
-- monsterAction gs = foldr individualMonsterAction gs (Array.fromFoldable $ Map.values gs.mobs)
--   where
--     individualMonsterAction :: Mob -> GameState -> GameState
--     individualMonsterAction mob gs' = let mobPos = position mob in
--       case playerAdjacent mobPos of
--         Just dir -> attackPlayer gs' mob
--         Nothing -> let
--             findEmptySpace :: Position -> Maybe Direction
--             findEmptySpace = findAdjacent findEmptySpaceDirection
--
--             findEmptySpaceDirection :: Position -> Direction -> Boolean
--             findEmptySpaceDirection pos dir = let targetPos = move dir gs'.atlas pos
--               -- in not $ blocksMovement $ getElement targetPos gs.atlas
--               in passable gs' targetPos
--           in case findEmptySpace mobPos of
--             Just moveDir ->
--               let gs'' = gs' { mobs = Map.delete mobPos gs'.mobs }
--                   targetPos = move moveDir gs'.atlas mobPos
--               in gs'' { mobs = Map.insert targetPos mob gs''.mobs }
--             Nothing -> gs'
--
--     playerAdjacent :: Position -> Maybe Direction
--     playerAdjacent = findAdjacent playerAdjacentDirection
--
--     playerAdjacentDirection :: Position -> Direction -> Boolean
--     playerAdjacentDirection pos dir = (move dir gs.atlas pos) == gs.player

doMobStuff :: GameState -> GameState
doMobStuff gs = flip execState gs $ (Map.keys gs.mobs) # traverse_ \mobPos -> do
  maction <- liftMobState mobPos (individualMonsterAction)
  case maction of
    Nothing -> pure unit
    Just action -> modify_ $ interpretMobAction action

interpretMobAction :: MobAction -> GameState -> GameState
interpretMobAction (MobAttack mob) gs = attackPlayer gs mob
interpretMobAction (MobPass mob) gs = gs

monsterMovement :: GameState -> Position -> Position
monsterMovement gs p = map (\d -> move d gs.atlas p) [U,D,L,R]
  # filter (passable gs)
  # cons' p
  # sortBy (comparing \x -> fromMaybe 100 $ Map.lookup x gs.distanceMap)
  # head

data MobAction = MobAttack Mob | MobPass Mob

individualMonsterAction :: GameState -> State Mob MobAction
individualMonsterAction gs = do
  mob <- get
  let mobPos = position mob
  case playerAdjacent mobPos of
    Just dir -> MobAttack <$> get
    Nothing -> do
        modify_ $ moveMob' (monsterMovement gs mobPos)
        MobPass <$> get
  where
    playerAdjacent :: Position -> Maybe Direction
    playerAdjacent = findAdjacent playerAdjacentDirection

    playerAdjacentDirection :: Position -> Direction -> Boolean
    playerAdjacentDirection pos dir = (move dir gs.atlas pos) == gs.player

    findEmptySpace :: Position -> Maybe Direction
    findEmptySpace = findAdjacent findEmptySpaceDirection

    findEmptySpaceDirection :: Position -> Direction -> Boolean
    findEmptySpaceDirection pos dir = let targetPos = move dir gs.atlas pos
      in passable gs targetPos


customers :: GameState -> GameState
customers = execState (liftCustomerState tickCustomers *> applyReward')
