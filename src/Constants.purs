module Constants where

import Extra.Prelude

font :: String
font = "16px monospace"

charWidth :: Number
charWidth = 10.0

charHeight :: Number
charHeight = 16.0

tileDimensions :: { width ∷ Int, height ∷ Int }
tileDimensions =
  { width: 32
  , height: 32
  }

displayDimensions :: { width ∷ Int, height ∷ Int }
displayDimensions = { width: 15, height: 15 }

canvasDimensions :: { width ∷ Number, height ∷ Number }
canvasDimensions = { width: 640.0, height: 480.0 }

newtype Color = Color String
derive instance newtypeColor :: Newtype Color _

black :: Color
black = Color "#000000"

white :: Color
white = Color "#FFFFFF"

gray :: Color
gray = Color "#C0C0C0"

blue :: Color
blue = Color "#0000FF"
