module Extra.Math where

import Prelude

import Data.Int (toNumber) as Int
import Data.Newtype (class Newtype)
import Math (pow, sqrt)

class Real a where
  toNumber :: a -> Number

instance realNumber :: Real Number where 
  toNumber = identity

instance realInt :: Real Int where
  toNumber = Int.toNumber

newtype Vector a = V { x :: a, y :: a }

derive newtype instance semigroupVector :: Semigroup a => Semigroup (Vector a)
derive newtype instance monoidVector :: Monoid a => Monoid (Vector a)
derive newtype instance semiringVector :: Semiring a => Semiring (Vector a)
derive newtype instance ringVector :: Ring a => Ring (Vector a)

derive instance eqVector :: Eq a => Eq (Vector a)
derive instance ordVector :: Ord a => Ord (Vector a)
derive instance newtypeVector :: Newtype (Vector a) _

instance functorVector :: Functor Vector where
  map f (V v) = V {x: f v.x, y: f v.y }

norm :: forall a. Real a => Vector a -> Number
norm (V v) = sqrt (pow (toNumber v.x) 2.0 + pow (toNumber v.y) 2.0)

innerProduct :: forall a. Semiring a => Vector a -> Vector a -> a
innerProduct (V v) (V w) = v.x * w.x + v.y * w.y

infixr 8 innerProduct as **
