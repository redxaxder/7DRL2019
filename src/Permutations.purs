module Permutations where

import Extra.Prelude

import Data.Array
  ( (..)
  , cons
  , drop
  , index
  , mapWithIndex
  , scanl
  , sort
  , take
  , unsafeIndex
  , zipWith
  )
import Data.Tuple (snd)


import Random (Random, intRange)

-- Permutation on S { 0 ... n }
data S = S Int (Int -> Int)

permute :: forall a. Partial => S -> Array a -> Array a
permute (S _ f) arr = unsafeIndex arr <<< f <$> (0 .. (length arr - 1))


instance showS :: Show S where
  show s = show (tabulate s)

instance monoidS :: Monoid S where
  mempty = S 0 identity

instance semigroupD :: Semigroup S where
  append (S bounda fa) (S boundb fb) = S (max bounda boundb) (fa <<< fb)

instance groupS :: Group S where
  invert = untabulate <<< invertArray <<< tabulate

tabulate :: S -> Array Int
tabulate (S bound f) = f <$> 0 .. bound

untabulate :: Array Int -> S
untabulate table = S (length table) $ \x -> (fromMaybe x $ index table x)

invertArray :: Array Int -> Array Int
invertArray a = map snd $ sort $ mapWithIndex (flip Tuple) a

transpose :: Int -> Int -> S
transpose i j = S (max i j) $ \x ->
  if x == i then j
    else if x == j then i
           else x

cycle :: Array Int -> S
cycle ints = foldr (<>) mempty $ zipWith transpose ints (drop 1 ints)

toS :: Int -> S
toS = fromLLehmer <<< fromInt

fromS :: S -> Int
fromS = toInt <<< toLLehmer


---------------------------------------------------
-- Lehmer Codes
---------------------------------------------------


-- Reveresd Lehmer code
-- corresponse to vector of left inversions of a permutation
newtype LLehmer = LLehmer (Array Int)

instance showLehmer :: Show LLehmer where
  show (LLehmer x) = show x

toLLehmer :: S -> LLehmer
toLLehmer (S bound f) =
  let z = 0 .. bound
      fa = f <$> z
      inversions = flip mapWithIndex fa $ \i fi -> countIf ((<) fi) (take i fa)
   in LLehmer $ inversions

toInt :: LLehmer -> Int
toInt (LLehmer arr) =
  let xs = facs (length arr)
      ys = drop 1 arr
   in sum $ zipWith (*) xs ys

fromInt :: Int -> LLehmer
fromInt x = LLehmer $ unfoldr f { i: x, n: 1 }
  where
  f {i, n} =
    if i == 0
      then Nothing
      else let d = i `div` n
               r = i `mod` n
            in Just $ r |> {i: d, n: n+1}

facs :: Int -> Array Int
facs n = scanl (*) 1 (1 .. n)

fromLLehmer :: LLehmer -> S
fromLLehmer (LLehmer []) = mempty
fromLLehmer (LLehmer arr) =
  let bound = length arr - 1
      pop i xs = { got: unsafeFromJust $ index xs i
                 , left: take i xs <> drop (i+1) xs
                 }
      step i { build, consume, n } =
        let { got, left } = pop (n -i) consume
         in { build: cons got build
            , consume: left
            , n: n - 1
            }
      table = (foldr step { build: [], consume: 0 .. bound, n: bound } arr).build
  in S bound (\i -> fromMaybe i $ index table i)


lehmerProduct :: Int -> Int -> Int
lehmerProduct x y = fromS $ toS x <> toS y

lehmerInverse :: Int -> Int
lehmerInverse = fromS <<< invert <<< toS


newtype L = L Int

instance showL :: Show L where
  show (L x) = "L " <> show x

instance semigroupL :: Semigroup L where
  append (L a) (L b) = L $ lehmerProduct a b

instance monoidL :: Monoid L where
  mempty = L 0

instance groupL :: Group L where
  invert (L x) = L $ lehmerInverse x


---------------------------------------------------------------
--  S5
---------------------------------------------------------------



----------------------------------------------------------
--  S8
----------------------------------------------------------

newtype S8 = S8 L

randomS8 :: Random S8
randomS8 = (S8 <<< L) <$> intRange 0 40319


