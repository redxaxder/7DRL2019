module Constants where

import Prelude

import Data.Int (toNumber)

font :: String
font = "16px monospace"

tileDimensions :: { width ∷ Int, height ∷ Int }
tileDimensions =
  { width: 16
  , height: 16
  }

displayDimensions :: { width ∷ Int, height ∷ Int }
displayDimensions =
  { width: 20
  , height: 20
  }

canvasDimensions :: { width ∷ Number, height ∷ Number }
canvasDimensions =
  { width: toNumber $ tileDimensions.width * displayDimensions.width
  , height: toNumber $ tileDimensions.height * displayDimensions.height
  }

