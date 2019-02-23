module Intent
  ( Action (..)
  , getAction
  ) where

import Extra.Prelude

import Direction (Direction (..))
import Types (GameState)

data Action = Move Direction

type Key = String
getAction :: GameState -> Key -> Maybe Action
getAction _ "ArrowLeft"  = Just (Move Left)
getAction _ "ArrowRight" = Just (Move Right)
getAction _ "ArrowDown"  = Just (Move Down)
getAction _ "ArrowUp"    = Just (Move Up)
getAction _ _            = Nothing

