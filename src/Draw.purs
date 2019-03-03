module Draw where

import Extra.Prelude

import Constants (canvasDimensions, tileDimensions)
import Data.Array.NonEmpty as NE
import Data.Map (toUnfoldable)
import Data.String.CodeUnits (singleton)
import Data.String.CodePoints as String
import FOV (scan)
import Graphics.Canvas (Context2D, fillRect, fillText)
import Graphics.Canvas (setFillStyle) as Canvas
import Tile (Tile(..))
import Types (GameState, Item, UIRenderData(..))

clear :: Context2D -> Effect Unit
clear ctx = do
  setFillStyle ctx black
  fillRect ctx { x: 0.0, y: 0.0, width: canvasDimensions.width, height: canvasDimensions.height }

clearRegion :: Context2D -> {x :: Number, y :: Number, width :: Number, height :: Number} -> Effect Unit
clearRegion ctx rect = do
  setFillStyle ctx black
  fillRect ctx rect

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
draw ctx (InventoryScreen i) gs = drawInventoryScreen ctx i gs
draw ctx _ gs = drawMain ctx gs


drawInventoryScreen :: Context2D -> Maybe { label:: Char, item :: Item } -> GameState  -> Effect Unit
drawInventoryScreen ctx Nothing gs = do
  let items = toUnfoldable gs.inventory :: Array (Tuple Char Item)
  drawText ctx "Inventory" 53.0 0.0
  void $ flip traverseWithIndex items \ix (Tuple c {name}) ->
    drawText ctx (singleton c <> ") " <> name) 53.0 (toNumber (tileDimensions.height * (ix + 1)))

drawInventoryScreen ctx (Just {label, item}) gs = do
  let items = toUnfoldable gs.inventory :: Array (Tuple Char Item)
  --clearRegion ctx { x: 53.0, y: 0.0, width: 1000.0, height: textOffset.y + charHeight * (length items + 1)}
  drawText ctx item.name 53.0 0.0

charWidth :: Number
charWidth = 10.0

charHeight :: Number
charHeight = 16.0

getTextDimensions :: String -> { width :: Number, height :: Number }
getTextDimensions t = { width: charWidth * (toNumber $ String.length t), height: charHeight }

drawText :: Context2D -> String -> Number -> Number -> Effect Unit
drawText ctx text x y = do
  let {width, height} = getTextDimensions text
  clearRegion ctx {x,y, width, height}
  setFillStyle ctx white
  fillText ctx text (textOffset.x + x) (textOffset.y + y)


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
textOffset = { x: 2.0, y: 12.0 }

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
  canvasy = toNumber $ (pos.y + 10) * tileDimensions.height
