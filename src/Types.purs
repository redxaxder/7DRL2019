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

import Control.Monad.State (State, modify_, get, runState)
import Data.Map (Map)
import Data.Map as Map
import Data.Array (catMaybes)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Lens.Record (prop)
import Data.Lens.Zoom (zoom)
import Data.Symbol (SProxy(..))

import Atlas (Atlas, Position)
import Data.Attribute (Attribute (..))
import Data.Maps (MapData (..))
import Data.Item (orderable)
import Data.Recipe (RecipeRecord)
import Data.Region (Region (..))
import Data.Sprite (Sprite (..))
import Data.Tile (Tile (..))
import Direction (Direction)
import Random (Gen)
import Random.Gen (Random, chance, element, intRange, runRandom)
import Types.Furniture (FurnitureType, Furniture (..))
import Types.Item (Item (..), ItemType)
import Types.Mob (Mob (..), MobType, position)

type GameState =
  { player :: Position
  , atlas :: Atlas Tile
  , inventory :: Map Char Item
  , items :: Map Position Item
  , customerState :: CustomerState
  , placeholders :: Map Position Placeholder
  , fov :: FieldOfView
  , mobs :: Map Position Mob
  , furniture :: Map Position Furniture
  , logevents :: Array LogEvent -- log all the events
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

data UIRenderData = MainGame
  | StartScreen
  | InventoryScreen (Maybe {label :: Char, item :: Item})
  | Crafting (Array { label :: Char, item :: Item }) (Array RecipeRecord)


type MapGenHint = { rng :: Gen, region :: Region }

type Placeholder = { position :: Position, direction :: Direction, next :: MapGenHint}

type FieldOfView = Array { screen :: Vector Int, absolute :: Position, tiles :: Array Tile }

data LogEvent = ItemEvent Item
              | CombatEvent Mob
              | MonsterKilledEvent Mob
              | PlayerAttacked Mob

liftAtlasState :: forall a. State (Atlas Tile) a -> State GameState a
liftAtlasState = zoom $ prop (SProxy :: SProxy "atlas")

liftCustomerState :: forall a. State CustomerState a -> State GameState a
liftCustomerState = zoom $ prop (SProxy :: SProxy "customerState")



type Customer =
    { order     :: ItemType
    , turnsLeft :: Int
    , reward    :: Reward
    }

rollCustomers :: Random (Array Customer)
rollCustomers = while (chance 2) (map pure rollCustomer)
  where

    rollOrder :: Random ItemType
    rollOrder = element orderable

    rollTurnsLeft :: ItemType -> Random Int
    rollTurnsLeft _ = intRange 100 200

    rollReward :: ItemType -> Random Reward
    rollReward _ = pure None

    rollCustomer :: Random Customer
    rollCustomer = do
      order <- rollOrder
      turnsLeft <- rollTurnsLeft order
      reward <- rollReward order
      pure { order, turnsLeft, reward }



data Reward = None | Multiple (Array Reward)
derive instance eqReward :: Eq Reward

applyReward :: Reward -> GameState -> GameState
applyReward None = identity
applyReward (Multiple rewards) =
  foldr (\reward f -> applyReward reward <<< f) identity rewards



newtype CustomerState = CustomerState { rng :: Gen, customers :: Array Customer }
derive instance newtypeCustomerState :: Newtype CustomerState _

serveCustomer :: Item -> State CustomerState (Maybe Reward)
serveCustomer item@(Item { itemType }) =
  zoom (prop (SProxy :: SProxy "customers") >>> _Newtype) do
    customer <- pop (\c -> itemType == c.order)
    pure (_.reward <$> customer)

customerStateFromGen :: Gen -> CustomerState
customerStateFromGen rng = CustomerState { rng, customers: [] }

tickCustomerState :: State CustomerState Unit
tickCustomerState = modify_ \(CustomerState { rng, customers }) ->
  let { result, nextGen } = rng # runRandom rollCustomers
  in CustomerState { rng: nextGen, customers: customers <> result }
