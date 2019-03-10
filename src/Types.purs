module Types
  ( module Types
  , module Data.Attribute
  , module Types.Furniture
  , module Types.Item
  , module Types.Mob
  , module Data.Region
  , module Data.Sprite
  , module Data.Maps
  , module Data.Tile
  )
  where

import Extra.Prelude

import Control.Monad.State (State, modify_, get, runState, put)
import Data.Array (catMaybes, find)
import Data.Map (Map)
import Data.Map as Map
import Data.Maybe (isJust)
import Data.Lens.Record (prop)
import Data.Lens.Zoom (zoom)
import Data.Symbol (SProxy(..))

import Atlas (Atlas, Position)
import Data.Attribute (Attribute (..))
import Data.Maps (MapData (..))
import Data.Recipe (RecipeRecord)
import Data.Region (Region (..))
import Data.Sprite (Sprite (..))
import Data.Tile (Tile (..))
import Direction (Direction)
import Random (Gen)
import Types.Customer (CustomerState(..), Reward(..))
import Types.Furniture (FurnitureType, Furniture (..))
import Types.Item (Item (..), ItemType)
import Types.Mob (Mob (..), MobType, position)

type GameState =
  { atlas :: Atlas Tile
  , customerState :: CustomerState
  , distanceMap :: Map Position Int
  , fov :: FieldOfView
  , furniture :: Map Position Furniture
  , inventory :: Map Char Item
  , items :: Map Position Item
  , logevents :: Array LogEvent -- log all the events
  , mobs :: Map Position Mob
  , placeholders :: Map Position Placeholder
  , player :: Position
  , startingPosition :: Position -- for warping
  }

liftMobState :: forall a. Position -> (GameState -> State Mob a) -> State GameState (Maybe a)
liftMobState p f = do
  gs <- get
  zoom (prop $ SProxy :: SProxy "mobs") $ do
    mobMap <- get
    case Map.lookup p mobMap of
      Nothing -> pure Nothing
      Just m -> let (Tuple a m') = runState (f gs) m
                    p' = position m'
                in if Map.member p' mobMap && (p' /= p)
                    then pure Nothing
                    else do
                      modify_ $ Map.delete p
                      modify_ $ Map.insert (position m') m'
                      pure (Just a)


-- TODO: where should this live?
getVisible :: forall a. FieldOfView -> Map Position a -> Array { a :: a, screen :: Vector Int }
getVisible fov m = catMaybes $ flip map fov $ \{ screen, absolute } ->
  map (\a -> { a, screen }) $ Map.lookup absolute m

data UIRenderData
  = MainGame (Array UIHint)
  | StartScreen
  | InventoryScreen (Maybe {label :: Char, item :: Item}) (Array UIHint)
  | ServeCustomerScreen (Array UIHint)
  | Crafting
      (Array { label :: Char, item :: Item })
      (Array RecipeRecord)
      (Array UIHint)
      (Maybe FurnitureType)

type MapGenHint = { rng :: Gen, region :: Region }

type Placeholder = { position :: Position, direction :: Direction, next :: MapGenHint}

type FieldOfView = Array { screen :: Vector Int, absolute :: Position, tiles :: Array Tile }

data LogEvent = ItemEvent Item
              | CombatEvent Mob
              | MonsterKilledEvent Mob
              | PlayerAttacked Mob

type Key = String
data UIHint = UIHint Key String

assembleUIHint :: UIHint -> String
assembleUIHint (UIHint "KeyC" string) = "(c) " <> string <> " "
assembleUIHint (UIHint "KeyD" string) = "(d) " <> string <> " "
assembleUIHint (UIHint "KeyI" string) = "(i) " <> string <> " "
assembleUIHint (UIHint "KeyW" string) = "(w) " <> string <> " "
assembleUIHint (UIHint "Escape" string) = "(esc) " <> string <> " "
assembleUIHint (UIHint "Space" string) = "(space) " <> string <> " "
assembleUIHint (UIHint "Period" string) = "(.) " <> string <> " "
assembleUIHint (UIHint desc string) = "(" <> desc <> ") " <> string <> " "

liftAtlasState :: forall a. State (Atlas Tile) a -> State GameState a
liftAtlasState = zoom $ prop (SProxy :: SProxy "atlas")

liftInventoryState :: forall a. State (Map Char Item) a -> State GameState a
liftInventoryState = zoom $ prop (SProxy :: SProxy "inventory")

liftCustomerState :: forall a. State CustomerState a -> State GameState a
liftCustomerState = zoom $ prop (SProxy :: SProxy "customerState")

applyReward :: Reward -> GameState -> GameState
applyReward None = identity
applyReward (Multiple rewards) =
  foldr (\reward f -> applyReward reward <<< f) identity rewards

applyReward' :: State GameState Unit
applyReward' = do
  gs <- get
  let CustomerState cs = gs.customerState
  cs.pending # maybe (pure unit) (modify_ <<< applyReward)
  put $ gs { customerState = CustomerState (cs { pending = Nothing }) }

canServe :: Item -> GameState -> Boolean
canServe item@(Item { itemType }) { customerState: CustomerState cs } =
  isJust $ find (\c -> itemType == c.order) cs.customers

data Action = Move Direction | Drop Char | Serve Char | Warp | Pass
