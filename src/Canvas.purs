module Canvas where

import Extra.Prelude

import Effect.Aff (Aff, makeAff)
import Effect.Class (liftEffect)
import Effect.Exception (error)
import Graphics.Canvas (CanvasImageSource, Context2D, setFont)
import Graphics.Canvas as Canvas
import Control.Monad.Maybe.Trans (runMaybeT, MaybeT (..))

import Constants (tileDimensions, canvasDimensions, font)

-- import Graphics.Canvas (getCanvasElementById, getContext2D, setCanvasDimensions, setFont) -- todo move this out of main?

getCanvasContext :: String -> Effect (Maybe Context2D)
getCanvasContext path = runMaybeT do
  canvas <- MaybeT $ Canvas.getCanvasElementById "game"
  liftEffect $ Canvas.setCanvasDimensions canvas canvasDimensions
  ctx <- liftEffect $ Canvas.getContext2D canvas
  liftEffect $ setFont ctx font
  pure ctx

loadImage :: String -> Aff CanvasImageSource
loadImage path = makeAff \handler -> do
  Canvas.tryLoadImage path (handler <<< maybe (Left $ error "failed to load image") pure)
  mempty

drawTile :: Context2D -> CanvasImageSource -> Sprite -> Vector Int -> Effect Unit
drawTile ctx tileset (Sprite { offsetX, offsetY }) (V { x, y }) =
  let
    { width, height } = tileDimensions
    sourceX = toNumber (offsetX * width)
    sourceY = toNumber (offsetY * height)
    canvasX = toNumber (x * width)
    canvasY = toNumber (y * height)
    w = toNumber width
    h = toNumber height
  in
  Canvas.drawImageFull ctx tileset sourceX sourceY w h canvasX canvasY w h

newtype Sprite = Sprite { offsetX :: Int, offsetY :: Int }

tile :: Int -> Int -> Sprite
tile offsetX offsetY = Sprite { offsetX, offsetY }

player :: Sprite
player = todo

counter :: Sprite
counter = todo

stove :: Sprite
stove = todo

oven :: Sprite
oven = todo

kitchenWall :: Sprite
kitchenWall = todo

kitchenFloor :: Sprite
kitchenFloor = todo
