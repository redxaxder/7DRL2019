module Types.Customer where

import Extra.Prelude

import Control.Monad.State (State, get, put, modify_)
import Data.Lens.Record (prop)
import Data.Lens.Zoom (zoom)
import Data.Lens.Iso.Newtype (_Newtype)
import Data.Symbol (SProxy(..))

import Data.Item (ItemType, orderable)
import Random (Gen)
import Random.Gen (Random, chance, element, intRange, runRandom)
import Types.Item (Item(..))

type Customer =
    { order     :: ItemType
    , turnsLeft :: Int
    , reward    :: Reward
    }

data Reward = None | Multiple (Array Reward)

derive instance eqReward :: Eq Reward

newtype CustomerState = CustomerState
  { rng :: Gen
  , customers :: Array Customer
  , pending :: Maybe Reward
  }

derive instance newtypeCustomerState :: Newtype CustomerState _

customerStateFromGen :: Gen -> CustomerState
customerStateFromGen rng = CustomerState { rng, customers: [], pending: Nothing }

serveCustomer :: Item -> State CustomerState Unit
serveCustomer item@(Item { itemType }) = do
  CustomerState cs <- get
  case cs.pending of
    Just _ -> pure unit
    Nothing -> do
      customer <- zoom
        (prop (SProxy :: SProxy "customers") >>> _Newtype)
        (pop (\c -> itemType == c.order))
      let maybeReward = map _.reward customer
      put <<< CustomerState $ cs { pending = maybeReward }

tickCustomers :: State CustomerState Unit
tickCustomers = modify_ \(CustomerState cs) ->
  let

    rollCustomers :: Random (Array Customer)
    rollCustomers = while (chance 2) (map pure rollCustomer)

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

    { result, nextGen } = cs.rng # runRandom rollCustomers

  in CustomerState
      { rng: nextGen
      , customers: cs.customers <> result
      , pending: cs.pending
      }
