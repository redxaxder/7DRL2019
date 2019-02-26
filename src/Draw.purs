module Draw where

import Extra.Prelude

import Data.Array.NonEmpty as NE
import Data.Map (toUnfoldable)
import Data.String.CodeUnits (singleton)
import Graphics.Canvas (Context2D, fillRect, fillText)
import Graphics.Canvas (setFillStyle) as Canvas

import Constants (canvasDimensions, tileDimensions)
import FOV (scan)
import Tile (Tile (..))
import Types (GameState, UIRenderData (..))

clear :: Context2D -> Effect Unit
clear ctx = do
  setFillStyle ctx black
  fillRect ctx { x: 0.0, y: 0.0, width: canvasDimensions.width, height: canvasDimensions.height }

newtype Color = Color String
derive instance newtypeColor :: Newtype Color _

black :: Color
black = Color "#000000"

white :: Color
white = Color "#FFFFFF"

setFillStyle :: Context2D -> Color -> Effect Unit
setFillStyle ctx (Color c) = Canvas.setFillStyle ctx c

type Glyph = { char :: Char, fgcolor :: Color, bgcolor :: Color }

visionRange :: Int -- TODO: move this to where it really lives
visionRange = 10

draw :: Context2D -> UIRenderData -> GameState -> Effect Unit
draw ctx StartScreen _ = drawStartScreen ctx
draw ctx _ gs = drawMain ctx gs

drawStartScreen :: Context2D -> Effect Unit
drawStartScreen ctx = do
  clear ctx
  setFillStyle ctx white
  fillText ctx "Press any key to start" 53.0 154.0

drawMain :: Context2D -> GameState -> Effect Unit
drawMain ctx gs = do
  clear ctx
  sequence_ $ flip map positions $ \(Tuple pos elements) ->
     drawGlyph ctx (getGlyph elements) pos
  drawGlyph ctx player zero
  where
  getGlyph xss = case NE.fromArray xss of
                   Nothing -> glitch
                   Just xs -> case NE.head xs of
                                   Wall -> wall
                                   _ -> floor
  player = { char: '@', fgcolor: white, bgcolor: black }
  wall   = { char: '#', fgcolor: white, bgcolor: black }
  floor  = { char: '.', fgcolor: white, bgcolor: black }
  glitch = { char: '/', fgcolor: white, bgcolor: black }
  positions :: Array (Tuple (Vector Int) (Array Tile))
  positions = toUnfoldable $ scan visionRange gs.player gs.atlas -- TODO: move this to where it really lives

textOffset :: { x ∷ Number, y ∷ Number }
textOffset = { x: 3.0, y: 14.0 }

drawGlyph :: Context2D -> Glyph -> Vector Int -> Effect Unit
drawGlyph ctx t (V pos) = do
  setFillStyle ctx t.bgcolor
  fillRect ctx
    { x: canvasx
    , y: canvasy
    , width: toNumber tileDimensions.width
    , height: toNumber tileDimensions.height
    }
  setFillStyle ctx t.fgcolor
  fillText ctx (singleton t.char) (canvasx + textOffset.x) (canvasy + textOffset.y)
  where
  canvasx = toNumber $ (pos.x + 10) * tileDimensions.width
  canvasy = toNumber $ (pos.y + 10)* tileDimensions.height
