module Random.Blob
  ( Blob
  , Ints (..)
  , Doubles (..)
  , fromInts
  , toInts
  , toDoubles
  , merge
  , perturb
  ) where

import Extra.Prelude

import Node.Buffer
  ( Buffer
  , BufferValueType (Int32LE)
  , concat
  , create
  , read
  , write
  )
import Effect.Unsafe (unsafePerformEffect)

import Random.Hash (Algorithm (SHA256), hash)

-- 256 bit (32 byte) binary blob
newtype Blob = Blob Buffer

merge :: Blob -> Blob -> Blob
merge (Blob b1) (Blob b2) = Blob $ hash SHA256
  (unsafePerformEffect $ concat [b1, b2])

perturb :: Blob -> Blob
perturb (Blob b) = Blob (hash SHA256 b)


fromInts :: Ints -> Blob
fromInts (Ints a b c d e f g h) = unsafePerformEffect $ do
  buffer <- create 32
  write Int32LE a 0 buffer
  write Int32LE b 4 buffer
  write Int32LE c 8 buffer
  write Int32LE d 12 buffer
  write Int32LE e 16 buffer
  write Int32LE f 20 buffer
  write Int32LE g 24 buffer
  write Int32LE h 28 buffer
  pure $ Blob buffer

toInts :: Blob -> Ints
toInts (Blob b) = unsafePerformEffect $ Ints
  <$> read Int32LE 0 b
  <*> read Int32LE 4 b
  <*> read Int32LE 8 b
  <*> read Int32LE 12 b
  <*> read Int32LE 16 b
  <*> read Int32LE 20 b
  <*> read Int32LE 24 b
  <*> read Int32LE 28 b


foreign import readDouble :: Int -> Buffer -> Effect Number

toDoubles :: Blob -> Doubles
toDoubles (Blob b) = unsafePerformEffect $ Doubles
  <$> readDouble 0 b
  <*> readDouble 8 b
  <*> readDouble 16 b
  <*> readDouble 24 b
