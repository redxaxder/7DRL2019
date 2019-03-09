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

import Control.Monad.State (State, get, modify_, put)
import Data.Map (Map)
import Data.Map as Map
import Data.Array (catMaybes, deleteAt, findIndex, index, (:))
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
import Random.Gen (Random, chance, element, runRandom)
import Types.Furniture (FurnitureType, Furniture (..))
import Types.Item (Item (..), ItemType)
import Types.Mob (Mob (..), MobType)

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

liftMobState :: forall a. Position -> State Mob a -> State GameState a
liftMobState p s = zoom (prop $ SProxy :: SProxy "mobs") 



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

{-
addToInventory :: Item -> State GameState Unit
addToInventory item = zoom (prop (SProxy :: SProxy "inventory")) $
  void $ modify (cons item)

addToInventory :: Item -> State GameState Unit
addToInventory item = zoom (prop (SProxy :: SProxy "inventory")) $
  void $ modify (delete item)
  -}

newtype CustomerState = CustomerState { rng :: Gen, customers :: Array Customer }
derive instance newtypeCustomerState :: Newtype CustomerState _

newtype Patience = Patience Int

type Customer =
    { order    :: ItemType
    , patience :: Patience
    , reward   :: Reward
    }

data Reward = None | Multiple (Array Reward)
derive instance eqReward :: Eq Reward

rollCustomers :: Random (Array Customer)
rollCustomers =
  while (chance 1) (map pure rollCustomer)
  where

    while :: forall m a. Monad m => Monoid a => m Boolean -> m a -> m a
    while mp ma = mp >>= \p -> if p then (<>) <$> ma <*> while mp ma else pure mempty

    rollPatience :: ItemType -> Random Patience
    rollPatience = todo

    rollReward :: ItemType -> Patience -> Random Reward
    rollReward = todo

    rollCustomer :: Random Customer
    rollCustomer = todo

pop :: forall a. (a -> Boolean) -> State (Array a) (Maybe a)
pop f = do
  arr <- get
  case findIndex f arr of
    Nothing -> pure Nothing
    Just i -> do
      put (unsafeFromJust $ deleteAt i arr)
      pure (index arr i)

-- applyReward :: Reward -> GameState -> GameState
-- applyReward None = identity
-- applyReward (Multiple rewards) = foldr identity (<<<) (applyReward <$> rewards)

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
