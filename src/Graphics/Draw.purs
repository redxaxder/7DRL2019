module Graphics.Draw where

import Extra.Prelude

import Data.Array (take, zip, range, cons, scanl, (..))
import Data.Array.NonEmpty as NE
import Data.String.CodeUnits (singleton)
import Data.Map (Map, toUnfoldable)
import Data.String.Common (toLower)

import Atlas (Position)
import Constants (displayDimensions, tileDimensions, white, canvasDimensions, charHeight)
import Data.Sprite (glitch, player, spriteAt)
import Data.Tile (Tile, tileSprite)
import Graphics.Render
  ( Context
  , drawSpriteToGrid
  , drawText
  , clear
  , setFillStyle
  , getTextDimensions
  , clearRegion
  , textOffset
  )
import Types (GameState, Item, UIRenderData(..), Sprite, getVisible, LogEvent(..), UIHint, assembleUIHint)
import Types.Furniture (furnitureSprite)
import Types.Item (itemSprite, itemName)
import Types.Mob (mobSprite, mobName)

draw :: Context -> UIRenderData -> GameState -> Effect Unit
draw ctx StartScreen _ = drawStartScreen ctx
draw ctx (InventoryScreen selected hints) gs =
  drawInventoryScreen ctx selected hints gs
draw ctx (MainGame hints) gs = drawMain ctx gs hints
draw ctx (Crafting selected recipes hints) gs = todo
draw ctx (ServeCustomerScreen sc) gs = todo

drawInventoryScreen :: Context -> Maybe { label:: Char, item :: Item } -> Array UIHint -> GameState  -> Effect Unit
drawInventoryScreen ctx Nothing hints gs = do
  let items = toUnfoldable gs.inventory :: Array (Tuple Char Item)
  drawUIHints ctx hints
  clearRegion ctx { x: uiLeftCoord, y: 0.0, width: 1000.0, height: textOffset.y + charHeight * (toNumber (length items + 1))}
  drawText ctx uiLeftCoord 0.0 "Inventory"
  void $ flip traverseWithIndex items \ix (Tuple c item) ->
    drawText ctx
    uiLeftCoord
    (toNumber (tileDimensions.height * (ix + 1)))
    (singleton c <> ") " <> itemName item)

drawInventoryScreen ctx (Just {label, item}) hints gs = do
  let items = toUnfoldable gs.inventory :: Array (Tuple Char Item)
  drawUIHints ctx hints
  clearRegion ctx { x: uiLeftCoord, y: 0.0, width: 1000.0, height: textOffset.y + charHeight * (toNumber (length items + 1))}
  drawText ctx
    uiLeftCoord
    0.0
    (itemName item)

drawStartScreen :: Context -> Effect Unit
drawStartScreen ctx = do
  clear ctx
  setFillStyle ctx white
  drawText ctx uiLeftCoord (154.0                   ) "You find an interdimensional dungeon"
  drawText ctx uiLeftCoord (154.0 +       charHeight) "entrance in the pantry of your"
  drawText ctx uiLeftCoord (154.0 + 2.0 * charHeight) "failing restaurant."
  drawText ctx uiLeftCoord (154.0 + 3.0 * charHeight) "The Monsters are delicious!"
  drawText ctx uiLeftCoord (154.0 + 4.0 * charHeight) "Go!  Save your restaurant!"

getUIHints :: UIRenderData -> Array UIHint
getUIHints (MainGame hints) = hints
getUIHints (InventoryScreen _ hints) = hints
getUIHints (Crafting _ _ hints) = hints
getUIHints StartScreen = mempty
getUIHints (ServeCustomerScreen hints) = hints

drawMain :: Context -> GameState -> Array UIHint -> Effect Unit
drawMain ctx gs hints = do
  clear ctx
  traverse_ (drawSpriteToGrid ctx (spriteAt 1 3)) do
    x <- 0 .. 14
    y <- 0 .. 14
    pure (V { x, y })
  gs.fov # traverse_ \{ screen, tiles } ->
    drawSpriteToGrid ctx (spriteFromTileStack tiles) (toCornerRelative screen)
  drawVisible gs.furniture furnitureSprite
  drawVisible gs.items itemSprite
  drawVisible gs.mobs mobSprite
  drawSpriteToGrid ctx player (toCornerRelative zero)
  drawLog ctx gs
  drawUIHints ctx hints
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
    drawLogItem (Tuple index evt) = drawText ctx (uiLeftCoord + 147.0) ((toNumber index) * charHeight) (logString evt)

    logString :: LogEvent -> String
    logString (ItemEvent item) = "Acquired " <> toLower (itemName item) <> "!"
    logString (CombatEvent mob) = "Hit " <> toLower (mobName mob) <> "!"
    logString (MonsterKilledEvent mob) = "Killed " <> toLower (mobName mob) <> "!"
    logString (PlayerAttacked mob) = "Hit by " <> toLower (mobName mob) <> "!"

logLines :: Int
logLines = 3

uiLeftCoord :: Number
uiLeftCoord = 53.0

uiHintScreenHeight :: Number
uiHintScreenHeight = canvasDimensions.height - charHeight * 3.0

drawUIHints :: Context -> Array UIHint -> Effect Unit
drawUIHints ctx hints =
  let widths = map (_.width <<< getTextDimensions <<< assembleUIHint) hints
      locations = cons 0.0 $ scanl (+) 0.0 widths
  in do
    clearRegion ctx {x: 0.0, y: uiHintScreenHeight, width: canvasDimensions.width, height: canvasDimensions.height}
    sequence_ $ map (drawHintItem ctx) (zip locations hints)

drawHintItem :: Context -> Tuple Number UIHint -> Effect Unit
drawHintItem ctx (Tuple loc hint) =
  drawText ctx loc uiHintScreenHeight $ assembleUIHint hint
