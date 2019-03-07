module Graphics.Draw where

import Extra.Prelude

import Data.Array.NonEmpty as NE
import Data.Map (Map, toUnfoldable, lookup)
import Data.String.CodeUnits (singleton)

import Atlas (Position(..))
import Constants (displayDimensions, tileDimensions, white)
import Graphics.Render
  ( Context
  , drawSpriteToGrid
  , drawText
  , clear
  , setFillStyle
  )
import Data.Sprite (glitch, player)
import Data.Furniture (furnitureSprite)
import Data.Tile (Tile, tileSprite)
import Types (GameState, Item, ItemName (..), UIRenderData(..), Sprite, Furniture,
  getVisible)

import Data.Item (itemSprite, itemName)
import Data.Mob (Mob)

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
    (singleton c <> ") " <> un ItemName (itemName item))
    53.0
    (toNumber (tileDimensions.height * (ix + 1)))

drawInventoryScreen ctx (Just {label, item}) gs = do
  let items = toUnfoldable gs.inventory :: Array (Tuple Char Item)
  --clearRegion ctx { x: 53.0, y: 0.0, width: 1000.0, height: textOffset.y + charHeight * (length items + 1)}
  drawText ctx 
    (un ItemName $ itemName item)
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
    drawSpriteToGrid ctx (spriteFromItem a) (toCornerRelative screen)
--  getVisible gs.fov gs.mobs # traverse_ \{ a: mob, screen } ->
--    drawSpriteToGrid ctx ( _.gfx mob ) (toCornerRelative screen)
  drawSpriteToGrid ctx player (toCornerRelative zero)
  pure unit
  where

  spriteFromTileStack :: Array Tile -> Sprite
  spriteFromTileStack xss = case NE.fromArray xss of
    Nothing -> glitch
    Just xs -> tileSprite $ NE.head xs

  spriteFromItem :: Item -> Sprite
  spriteFromItem _ = glitch

  drawItem :: Position -> Item -> Effect Unit
  drawItem (Position { localPosition }) item =
    drawSpriteToGrid ctx (itemSprite item) (toCornerRelative localPosition)
  toCornerRelative :: Vector Int -> Vector Int
  toCornerRelative (V {x,y}) = V { x: x', y: y' }
    where
    x' = x + div displayDimensions.width 2
    y' = y + div displayDimensions.height 2
