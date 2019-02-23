module Random.Hash
  ( Algorithm (..)
  , hash
  ) where

import Extra.Prelude

import Node.Buffer (Buffer)


data Algorithm
  = MD5
  | SHA256
  | SHA512

instance showAlgorithm :: Show Algorithm where
  show MD5 = "md5"
  show SHA256 = "sha256"
  show SHA512 = "sha512"

foreign import _hash :: String -> Buffer -> Buffer

hash :: Algorithm -> Buffer -> Buffer
hash a = _hash (show a)
