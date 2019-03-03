module Extra.Prelude
  ( module Prelude
  , module Data.Either
  , module Data.Foldable
  , module Data.Maybe
  , module Data.Newtype
  , module Data.Traversable
  , module Data.TraversableWithIndex
  , module Data.Tuple
  , module Data.Unfoldable
  , module Debug.Trace
  , module Effect
  , module Extra.Math

  , class Group
  , invert

  , (|>)
  , countIf
  , filterSet
  , foldl1
  , foldr1
  , foreach
  , groupBy'
  , groupToMap
  , keyBy
  , todo
  , unsafeFromJust
  ) where

import Prelude

import Data.Array (cons, groupBy, singleton, sortBy)
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Either (Either(..))
import Data.Foldable (class Foldable, null, length, foldr, foldl, sum)
import Data.Map (Map, alter, empty)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, maybe)
import Data.Newtype (class Newtype, unwrap, un)
import Data.Semigroup.Foldable (class Foldable1, foldMap1)
import Data.Set (Set)
import Data.Set as S
import Data.Traversable (class Traversable, traverse, traverse_, sequence_, sequence)
import Data.TraversableWithIndex (traverseWithIndex)
import Data.Tuple (Tuple(..))
import Data.Unfoldable (unfoldr)
import Debug.Trace (trace)
import Effect (Effect)
import Extra.Math (class Real, Vector(..), innerProduct, norm, toNumber, (**))
import Partial (crash)
import Partial.Unsafe (unsafePartial)
import Prim.TypeError (class Warn, Text)


infixr 0 Tuple as |>

newtype SG a = SG ((a -> a -> a) -> a)

derive instance newtypeSG :: Newtype (SG a) _

instance semigroupSG :: Semigroup (SG a) where
  append (SG x) (SG y) = SG (\f -> f (x f) (y f))

foldr1 :: forall f a . Foldable1 f => (a -> a -> a) -> f a -> a
foldr1 f xs = unwrap (foldMap1 (SG <<< const)  xs) f

foldl1 :: forall f a . Foldable1 f => (a -> a -> a) -> f a -> a
foldl1 f = foldr1 (flip f)

foreach :: forall t m a b
  . Foldable t
  => Applicative m
  => t a
  -> (a -> m b)
  -> m Unit
foreach = flip traverse_

groupBy' :: forall a. (a -> a -> Ordering) -> Array a -> Array (NonEmptyArray a)
groupBy' f xs =  groupBy ((map <<< map) (_ == EQ) f) <<< sortBy f $ xs

keyBy :: forall f k a. Foldable f => Ord k => (a -> k) -> f a -> Map k (Array a)
keyBy k = foldr (\v m -> alter (combine v) (k v) m) empty
  where
  combine :: a -> Maybe (Array a) -> Maybe (Array a)
  combine v (Just v') = Just $ cons v v'
  combine v Nothing = Just $ singleton v


groupToMap :: forall f k a b
  . Foldable f => Ord k
 => { key :: a -> k
    , value :: a -> b
    , merge :: b -> b -> b
    }
 -> f a
 -> Map k b
groupToMap {key, value, merge} = foldr (\v m -> alter (combine v) (key v) m) empty
  where
  combine :: a -> Maybe b -> Maybe b
  combine v (Just v') = Just $ merge (value v) v'
  combine v Nothing = Just $ value v


filterSet :: forall a. Ord a => (a -> Boolean) -> Set a -> Set a
filterSet f = flip foldr S.empty $ \x set ->
  if f x
    then S.insert x set
    else set

todo :: forall a. Warn (Text "Not implemented") => a
todo = unsafePartial $ crash "Not implemented"

countIf :: forall a t. Foldable t => (a -> Boolean) -> t a -> Int
countIf f xs = foldr g 0 xs
  where g x acc = if f x then acc + 1 else acc


unsafeFromJust :: forall a. Maybe a -> a
unsafeFromJust x = unsafePartial (fromJust x)


class Monoid a <= Group a where
  invert :: a -> a

