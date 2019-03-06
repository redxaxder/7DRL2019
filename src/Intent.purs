module Intent
  ( Action (..)
  , getAction
  ) where

import Extra.Prelude

import Direction (Direction (..))
import Types (GameState)

data Action = Move Direction | Drop Char | Pass

type Key = String
getAction :: GameState -> Key -> Maybe Action
getAction _ "ArrowLeft"  = Just (Move L)
getAction _ "ArrowRight" = Just (Move R)
getAction _ "ArrowDown"  = Just (Move D)
getAction _ "ArrowUp"    = Just (Move U)
getAction _ _            = Nothing
