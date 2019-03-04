module Constants where

import Extra.Prelude

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

newtype Color = Color String
derive instance newtypeColor :: Newtype Color _

black :: Color
black = Color "#000000"

white :: Color
white = Color "#FFFFFF"

blue :: Color
blue = Color "#FF0000"
