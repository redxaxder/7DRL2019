module Graphics.Draw where

import Extra.Prelude

import Data.Array.NonEmpty as NE
import Data.Map (toUnfoldable)
import Data.String.CodeUnits (singleton)

import Constants (displayDimensions, tileDimensions, white)
import Graphics.Render
  ( Context
  , drawSpriteToGrid
  , drawText
  , clear
  , setFillStyle
  )
import Graphics.Sprite (Sprite, floor, glitch, player, wall)
import FOV (scan)
import Tile (Tile(..))
import Types (GameState, Item, UIRenderData(..))

visionRange :: Int -- TODO: move this to where it really lives
visionRange = 10

draw :: Context -> UIRenderData -> GameState -> Effect Unit
draw ctx StartScreen _ = drawStartScreen ctx
draw ctx (InventoryScreen i) gs = drawInventoryScreen ctx i gs
draw ctx _ gs = drawMain ctx gs

drawInventoryScreen :: Context -> Maybe { label:: Char, item :: Item } -> GameState  -> Effect Unit
drawInventoryScreen ctx Nothing gs = do
  let items = toUnfoldable gs.inventory :: Array (Tuple Char Item)
  drawText ctx "Inventory" 53.0 0.0
  void $ flip traverseWithIndex items \ix (Tuple c {name}) ->
    drawText ctx (singleton c <> ") " <> name) 53.0 (toNumber (tileDimensions.height * (ix + 1)))

drawInventoryScreen ctx (Just {label, item}) gs = do
  let items = toUnfoldable gs.inventory :: Array (Tuple Char Item)
  --clearRegion ctx { x: 53.0, y: 0.0, width: 1000.0, height: textOffset.y + charHeight * (length items + 1)}
  drawText ctx item.name 53.0 0.0

drawStartScreen :: Context -> Effect Unit
drawStartScreen ctx = do
  clear ctx
  setFillStyle ctx white
  drawText ctx "Press any key to start" 53.0 154.0

drawMain :: Context -> GameState -> Effect Unit
drawMain ctx gs = do
  clear ctx
  tiles # traverse_ \(Tuple pos stack) ->
    drawSpriteToGrid ctx (spriteFromTileStack stack) (toScreenRelative pos)
  gs.items # traverse_ drawItem
  drawSpriteToGrid ctx player (toScreenRelative zero)
  where

  spriteFromTileStack :: Array Tile -> Sprite
  spriteFromTileStack xss = case NE.fromArray xss of
    Nothing -> glitch
    Just xs -> case NE.head xs of
      Wall -> wall
      _ -> floor

  drawItem :: _ -> Effect Unit
  drawItem = todo

  tiles :: Array (Tuple (Vector Int) (Array Tile))
  tiles = toUnfoldable $ scan visionRange gs.player gs.atlas -- TODO: move this to where it really lives

  toScreenRelative :: Vector Int -> Vector Int
  toScreenRelative (V {x,y}) = V { x: x', y: y' }
    where
    x' = x + div displayDimensions.width 2
    y' = y + div displayDimensions.height 2
