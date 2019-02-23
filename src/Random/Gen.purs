module Random.Gen
  ( Gen
  , Random (..)
  , branch
  , chance
  , intRange
  , newGen
  , next
  , nextDouble
  , nextDoubles
  , nextInt
  , nextInts
  , runRandom
  , split
  ) where

import Extra.Prelude

import Effect.Random (randomInt)
import Data.Int (floor)

import Random.Blob (Blob, Ints (..), Doubles(..), toInts, toDoubles, fromInts, perturb, merge)

newGen :: Effect Gen
newGen = map (Gen <<< fromInts) $
  Ints
  <$> randomInt bottom top
  <*> randomInt bottom top
  <*> randomInt bottom top
  <*> randomInt bottom top
  <*> randomInt bottom top
  <*> randomInt bottom top
  <*> randomInt bottom top
  <*> randomInt bottom top

newtype Gen = Gen Blob

nextInts :: Random Ints
nextInts = toInts <$> nextBlob

nextInt :: Random Int
nextInt = firstInt <$> nextInts
  where
  firstInt (Ints x _ _ _ _ _ _ _) = x

nextDoubles :: Random Doubles
nextDoubles = toDoubles <$> nextBlob

nextDouble :: Random Number
nextDouble = firstDouble <$> nextDoubles
  where
  firstDouble (Doubles x _ _ _) = x

nextBlob :: Random Blob
nextBlob = Random $ \(Gen blob) ->
   { result: blob
   , nextGen: Gen $ perturb blob
   }

next :: Random Unit
next = void nextBlob

split :: Gen -> { one :: Gen, two :: Gen }
split g@(Gen blob) =
  let b0 = merge blob blob
      b1 = merge b0 blob
      b2 = merge blob b0
   in { one: Gen b1
      , two: Gen b2
      }

branch :: Random Gen
branch = Random $ \gen ->
  let {one, two} = split gen
   in { result: one
      , nextGen: two
      }

chance :: Int -> Random Boolean
chance p = (>) p <$> intRange 0 100

intRange :: Int -> Int -> Random Int
intRange low high = flip map nextDouble $ \d ->
  let frac = d - toNumber (floor d)
      range = toNumber $ high - low
   in floor (range * frac) + low

newtype Random a = Random (Gen -> { result :: a, nextGen :: Gen })

runRandom :: forall a. Random a -> Gen -> { result :: a, nextGen :: Gen}
runRandom = un Random

instance functorRandom :: Functor Random where
  map f (Random r) =
    Random $ \gen ->
      let x = r gen
       in { result: f x.result
          , nextGen: x.nextGen
          }

derive instance newtypeRandom :: Newtype (Random a) _

instance applyRandom :: Apply Random where
  apply = ap

instance applicativeRandom :: Applicative Random where
  pure x = Random $ \gen -> { result: x, nextGen: gen }

instance bindRandom :: Bind Random where
  bind (Random x) f = Random $ \gen ->
    let r = x gen
     in runRandom (f r.result) r.nextGen

instance monadRandom :: Monad Random where
