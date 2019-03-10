module Intent where

import Direction (Direction)
import Types (GameState)

data Action = Move Direction | Drop Char | Serve Char | Pass
