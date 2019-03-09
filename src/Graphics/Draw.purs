module Graphics.Draw where

import Data.Array (take, zip, range)
import Data.Array.NonEmpty as NE
import Data.Map (toUnfoldable)
import Data.String.CodeUnits (singleton)
import Data.String.Common (toLower)

import Extra.Prelude

import Data.Map (Map, toUnfoldable)

import Atlas (Position)
import Constants (displayDimensions, tileDimensions, white)
import Data.Sprite (glitch, player)
import Data.Tile (Tile, tileSprite)
import Graphics.Render (Context, drawSpriteToGrid, drawText, clear, setFillStyle)
import Types (GameState, Item, UIRenderData(..), Sprite, getVisible, LogEvent(..))
import Types.Furniture (furnitureSprite)
import Types.Item (itemSprite, itemName)
import Types.Mob (mobSprite, mobName)

visionRange :: Int -- TODO: move this to where it really lives
visionRange = 10

draw :: Context -> UIRenderData -> GameState -> Effect Unit
draw ctx StartScreen _ = drawStartScreen ctx
draw ctx (InventoryScreen i) gs = drawInventoryScreen ctx i gs
draw ctx _ gs = drawMain ctx gs

drawInventoryScreen :: Context -> Maybe { label:: Char, item :: Item } -> GameState  -> Effect Unit
drawInventoryScreen ctx Nothing gs = do
  let items = toUnfoldable gs.inventory :: Array (Tuple Char Item)
  drawText ctx "Inventory" leftMargin 0.0
  void $ flip traverseWithIndex items \ix (Tuple c item) ->
    drawText ctx
    (singleton c <> ") " <> itemName item)
    leftMargin
    (toNumber (tileDimensions.height * (ix + 1)))

drawInventoryScreen ctx (Just {label, item}) gs = do
  let items = toUnfoldable gs.inventory :: Array (Tuple Char Item)
  --clearRegion ctx { x: leftMargin, y: 0.0, width: 1000.0, height: textOffset.y + charHeight * (length items + 1)}
  drawText ctx
    (itemName item)
    leftMargin
    0.0

drawStartScreen :: Context -> Effect Unit
drawStartScreen ctx = do
  clear ctx
  setFillStyle ctx white
  drawText ctx "Press any key to start" leftMargin 154.0

drawMain :: Context -> GameState -> Effect Unit
drawMain ctx gs = do
  clear ctx
  gs.fov # traverse_ \{ screen, tiles } ->
    drawSpriteToGrid ctx (spriteFromTileStack tiles) (toCornerRelative screen)
  drawVisible gs.furniture furnitureSprite
  drawVisible gs.items itemSprite
  drawVisible gs.mobs mobSprite
  drawSpriteToGrid ctx player (toCornerRelative zero)
  drawLog ctx gs
  pure unit
  where

  drawVisible :: forall a. Map Position a -> (a -> Sprite) -> Effect Unit
  drawVisible m sprite = getVisible gs.fov m # traverse_ \{ a, screen } ->
    drawSpriteToGrid ctx (sprite a) (toCornerRelative screen)

  spriteFromTileStack :: Array Tile -> Sprite
  spriteFromTileStack xss = case NE.fromArray xss of
    Nothing -> glitch
    Just xs -> tileSprite $ NE.head xs

  toCornerRelative :: Vector Int -> Vector Int
  toCornerRelative (V {x,y}) = V { x: x', y: y' }
    where
    x' = x + div displayDimensions.width 2
    y' = y + div displayDimensions.height 2

drawLog :: Context -> GameState -> Effect Unit
drawLog ctx gs = sequence_ $ map drawLogItem (zip (range 1 logLines) (take logLines gs.logevents))
  where
    drawLogItem :: Tuple Int LogEvent -> Effect Unit
    drawLogItem (Tuple index evt) = drawText ctx (logString evt) leftMargin (toNumber (index * 15))

    logString :: LogEvent -> String
    logString (ItemEvent item) = "Acquired " <> toLower (itemName item) <> "!"
    logString (CombatEvent mob) = "Hit " <> toLower (mobName mob) <> "!"
    logString (MonsterKilledEvent mob) = "Killed " <> toLower (mobName mob) <> "!"
    logString (PlayerAttacked mob) = "Hit by " <> toLower (mobName mob) <> "!"

  {-case gs.logevent of
  Just (ItemEvent item) -> drawText ctx ("Acquired " <> itemName item <> "!") leftMargin 0.0
  _ -> pure unit-}

logLines :: Int
logLines = 3

leftMargin :: Number
leftMargin = 53.0
