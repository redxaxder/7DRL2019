module Graphics.Render where

import Extra.Prelude

import Constants (tileDimensions, canvasDimensions, font, white, black, Color(..), displayDimensions)
import Control.Monad.Maybe.Trans (runMaybeT, MaybeT(..))
import Control.Monad.Trans.Class (lift)
import Effect.Aff (Aff, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Data.String.CodePoints as String
import Graphics.Canvas as Canvas
import Types (Sprite (..))

newtype Context = Context { context :: Canvas.Context2D, spritesheet :: Canvas.CanvasImageSource }

initCanvas :: { canvasId :: String, spritesheetPath :: String } -> Aff (Maybe Context)
initCanvas { canvasId, spritesheetPath } = runMaybeT do
  canvas <- MaybeT $ liftEffect $ Canvas.getCanvasElementById canvasId
  liftEffect $ Canvas.setCanvasDimensions canvas canvasDimensions
  context <- liftEffect $ Canvas.getContext2D canvas
  liftEffect $ Canvas.setFont context font
  spritesheet <- lift $ makeAff \handler -> do
    Canvas.tryLoadImage spritesheetPath (handler <<< maybe (Left $ error "failed to load image") pure)
    mempty
  pure $ Context { context, spritesheet }

drawSprite :: Context -> Sprite -> Vector Number -> Effect Unit
drawSprite (Context {context, spritesheet}) (Sprite { offsetX, offsetY }) (V { x, y }) =
  let
    { width, height } = tileDimensions
    sourceX = toNumber (offsetX * width)
    sourceY = toNumber (offsetY * height)
    w = toNumber width
    h = toNumber height
  in
  Canvas.drawImageFull context spritesheet sourceX sourceY w h x y w h

drawSpriteToGrid :: Context -> Sprite -> Vector Int -> Effect Unit
drawSpriteToGrid (Context {context, spritesheet}) (Sprite { offsetX, offsetY }) (V { x, y }) =
  let
    { width, height } = tileDimensions
    sourceX = toNumber (offsetX * width)
    sourceY = toNumber (offsetY * height)
    canvasX = 180.0 + toNumber (x * width)
    canvasY = toNumber (y * height)
    w = toNumber width
    h = toNumber height
  in
  when
    ( 0 <= x && x < displayDimensions.width
      && 0 <= y && y < displayDimensions.height
      )
    (Canvas.drawImageFull context spritesheet sourceX sourceY w h canvasX canvasY w h)

charWidth :: Number
charWidth = 10.0

charHeight :: Number
charHeight = 16.0

getTextDimensions :: String -> { width :: Number, height :: Number }
getTextDimensions t = { width: charWidth * (toNumber $ String.length t), height: charHeight }

drawText :: Context -> Number -> Number -> String -> Effect Unit
drawText ctx@(Context {context}) x y text = do
  let {width, height} = getTextDimensions text
  clearRegion ctx {x,y, width, height}
  setFillStyle ctx white
  Canvas.fillText context text (textOffset.x + x) (textOffset.y + y)

clear :: Context -> Effect Unit
clear ctx@(Context{context}) = do
  setFillStyle ctx black
  Canvas.fillRect context { x: 0.0, y: 0.0, width: canvasDimensions.width, height: canvasDimensions.height }

clearRegion :: Context -> {x :: Number, y :: Number, width :: Number, height :: Number} -> Effect Unit
clearRegion ctx@(Context {context}) rect = do
  setFillStyle ctx black
  Canvas.fillRect context rect

setFillStyle :: Context -> Color -> Effect Unit
setFillStyle (Context {context}) (Color c) = Canvas.setFillStyle context c

textOffset :: { x ∷ Number, y ∷ Number }
textOffset = { x: 3.0, y: 12.0 }
