module Graphics.Draw where

import Extra.Prelude

import Constants (displayDimensions, tileDimensions, white)
import Data.Array.NonEmpty as NE
import Data.Map (toUnfoldable)
import Types.Mob (mobSprite)
import Data.Sprite (glitch, player)
import Types.Furniture (furnitureSprite)
import Data.String.CodeUnits (singleton)
import Data.Tile (Tile, tileSprite)
import Types (GameState, Item, UIRenderData(..), Sprite, getVisible)
import Graphics.Render (Context, drawSpriteToGrid, drawText, clear, setFillStyle)
import Types.Item (itemSprite, itemName)
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
  void $ flip traverseWithIndex items \ix (Tuple c item) ->
    drawText ctx
    (singleton c <> ") " <> itemName item)
    53.0
    (toNumber (tileDimensions.height * (ix + 1)))

drawInventoryScreen ctx (Just {label, item}) gs = do
  let items = toUnfoldable gs.inventory :: Array (Tuple Char Item)
  --clearRegion ctx { x: 53.0, y: 0.0, width: 1000.0, height: textOffset.y + charHeight * (length items + 1)}
  drawText ctx
    (itemName item)
    53.0
    0.0

drawStartScreen :: Context -> Effect Unit
drawStartScreen ctx = do
  clear ctx
  setFillStyle ctx white
  drawText ctx "Press any key to start" 53.0 154.0

drawMain :: Context -> GameState -> Effect Unit
drawMain ctx gs = do
  clear ctx
  gs.fov # traverse_ \{ screen, tiles } ->
    drawSpriteToGrid ctx (spriteFromTileStack tiles) (toCornerRelative screen)
  getVisible gs.fov gs.furniture # traverse_ \{ a, screen } ->
    drawSpriteToGrid ctx (furnitureSprite a) (toCornerRelative screen)
  getVisible gs.fov gs.items # traverse_ \{ a, screen } ->
    drawSpriteToGrid ctx (itemSprite a) (toCornerRelative screen)
  getVisible gs.fov gs.mobs # traverse_ \{ a, screen } ->
    drawSpriteToGrid ctx (mobSprite a) (toCornerRelative screen)
  drawSpriteToGrid ctx player (toCornerRelative zero)
  pure unit
  where

  spriteFromTileStack :: Array Tile -> Sprite
  spriteFromTileStack xss = case NE.fromArray xss of
    Nothing -> glitch
    Just xs -> tileSprite $ NE.head xs

  toCornerRelative :: Vector Int -> Vector Int
  toCornerRelative (V {x,y}) = V { x: x', y: y' }
    where
    x' = x + div displayDimensions.width 2
    y' = y + div displayDimensions.height 2
