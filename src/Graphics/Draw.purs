module Graphics.Draw where

import Extra.Prelude

import Data.Array (take, zip, range, cons, scanl)
import Data.Array.NonEmpty as NE
import Data.String.CodeUnits (singleton)
import Data.Map (Map, toUnfoldable)
import Data.String.Common (toLower)

import Atlas (Position)
import Constants (displayDimensions, tileDimensions, white, canvasDimensions)
import Data.Sprite (glitch, player)
import Data.Tile (Tile, tileSprite)
import Graphics.Render (Context, drawSpriteToGrid, drawText, clear, setFillStyle, getTextDimensions, charHeight, clearRegion)
import Types (GameState, Item, UIRenderData(..), Sprite, getVisible, LogEvent(..), UIHint, assembleUIHint)
import Types.Furniture (furnitureSprite)
import Types.Item (itemSprite, itemName)
import Types.Mob (mobSprite, mobName)

visionRange :: Int -- TODO: move this to where it really lives
visionRange = 10

draw :: Context -> UIRenderData -> GameState -> Effect Unit
draw ctx StartScreen _ = drawStartScreen ctx
draw ctx ui@(InventoryScreen i _) gs = drawInventoryScreen ctx i (getUIHints ui) gs
draw ctx ui gs = drawMain ctx ui gs

drawInventoryScreen :: Context -> Maybe { label:: Char, item :: Item } -> Array UIHint -> GameState  -> Effect Unit
drawInventoryScreen ctx Nothing hints gs = do
  let items = toUnfoldable gs.inventory :: Array (Tuple Char Item)
  drawUIHints ctx hints
  drawText ctx uiLeftCoord 0.0 "Inventory" 
  void $ flip traverseWithIndex items \ix (Tuple c item) ->
    drawText ctx
    uiLeftCoord
    (toNumber (tileDimensions.height * (ix + 1)))
    (singleton c <> ") " <> itemName item)

drawInventoryScreen ctx (Just {label, item}) hints gs = do
  let items = toUnfoldable gs.inventory :: Array (Tuple Char Item)
  drawUIHints ctx hints
  --clearRegion ctx { x: uiLeftCoord, y: 0.0, width: 1000.0, height: textOffset.y + charHeight * (length items + 1)}
  drawText ctx
    uiLeftCoord
    0.0
    (itemName item)

drawStartScreen :: Context -> Effect Unit
drawStartScreen ctx = do
  clear ctx
  setFillStyle ctx white
  drawText ctx uiLeftCoord 154.0 "Press any key to start" 

getUIHints :: UIRenderData -> Array UIHint
getUIHints (MainGame hints) = hints
getUIHints (InventoryScreen _ hints) = hints
getUIHints (Crafting _ _ hints) = hints
getUIHints _ = mempty

drawMain :: Context -> UIRenderData -> GameState -> Effect Unit
drawMain ctx ui gs = do
  clear ctx
  gs.fov # traverse_ \{ screen, tiles } ->
    drawSpriteToGrid ctx (spriteFromTileStack tiles) (toCornerRelative screen)
  drawVisible gs.furniture furnitureSprite
  drawVisible gs.items itemSprite
  drawVisible gs.mobs mobSprite
  drawSpriteToGrid ctx player (toCornerRelative zero)
  drawLog ctx gs
  drawUIHints ctx (getUIHints ui)
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
    drawLogItem (Tuple index evt) = drawText ctx uiLeftCoord ((toNumber index) * charHeight) (logString evt)

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
