module Graphics.Draw where

import Extra.Prelude

import Control.Monad.State (evalState)
import Data.Array (take, zip, range, cons, scanl, (..))
import Data.Array.NonEmpty as NE
import Data.Foldable (maximum)
import Data.String.CodeUnits (singleton)
import Data.Map (Map, toUnfoldable)
import Data.String.Common (toLower)

import Atlas (Position)
import Constants (canvasDimensions, charHeight, displayDimensions, white)
import Data.Furniture (getFurnitureRecord)
import Data.Item (getItemRecord)
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
import Types
  ( GameState
  , Item
  , LogEvent(..)
  , Sprite
  , UIHint
  , UIRenderData(..)
  , assembleUIHint
  , getVisible
  , liftCustomerState
  )
import Types.Customer (Customer, getCustomers, displayReward)
import Types.Furniture (furnitureSprite)
import Types.Item (itemSprite, itemName)
import Types.Mob (mobSprite, mobName)

draw :: Context -> UIRenderData -> GameState -> Effect Unit

draw ctx (InventoryScreen selected hints) gs =
  drawModal ctx
    { title: "Inventory"
    , selected: maybe [] pure $
        map (\x -> { label: x.label, text: itemName x.item }) selected
    , columns: pure $
        map (\(Tuple label item) -> { label, text: itemName item }) $
        toUnfoldable gs.inventory :: Array (Tuple Char Item)
    , hints
    }

draw ctx (Crafting selected recipes hints furniture) gs =
  drawModal ctx
    { title:
        let getName = _.name <<< getFurnitureRecord
        in "Recipes (" <> maybe "Hand" getName furniture <> ")"
    , selected: []
    , columns: []
    , hints
    }

draw ctx (ServeCustomerScreen hints) gs =
  drawModal ctx
    { title: "Serve Customers"
    , selected: []
    , columns: []
    , hints
    }

draw ctx StartScreen _ = drawStartScreen ctx
draw ctx (MainGame hints) gs = drawMain ctx gs hints

drawModal ::
  Context ->
  { title :: String
  , selected :: Array { label :: Char, text :: String }
  , columns :: Array (Array { label :: Char, text :: String })
  , hints :: Array UIHint
  } ->
  Effect Unit
drawModal ctx props = do
  -- clear the log area plus any additional space needed
  let lines = max logLines $ fromMaybe 0 $ maximum $ map length props.columns
  clearRegion ctx
    { x: uiLeftCoord
    , y: 0.0
    , width: 1000.0
    , height: textOffset.y + charHeight * (toNumber $ lines + 1)
    }
  -- draw title
  drawText ctx (uiLeftCoord + 195.0) uiTopCoord props.title
  -- draw selected in a column
  drawText ctx uiLeftCoord (uiTopCoord + charHeight) "Selected"
  props.selected # traverseWithIndex_ \ix { label, text } ->
    drawText ctx
      uiLeftCoord
      (uiTopCoord + charHeight * toNumber (ix + 2))
      (singleton label <> ") " <> text)
  -- draw remaining columns
  props.columns # traverseWithIndex_ \colNo colDat ->
    colDat # traverseWithIndex_ \rowNo { label, text } ->
      drawText ctx
        (uiLeftCoord + 195.0 * toNumber (colNo + 1))
        (uiTopCoord + charHeight * toNumber (rowNo + 2))
        (singleton label <> ") " <> text)
  -- draw hints
  drawUIHints ctx props.hints

drawStartScreen :: Context -> Effect Unit
drawStartScreen ctx = do
  clear ctx
  setFillStyle ctx white
  drawText ctx uiLeftCoord (154.0                   ) "You find an interdimensional dungeon"
  drawText ctx uiLeftCoord (154.0 +       charHeight) "entrance in the pantry of your"
  drawText ctx uiLeftCoord (154.0 + 2.0 * charHeight) "failing restaurant."
  drawText ctx uiLeftCoord (154.0 + 3.0 * charHeight) "The Monsters are delicious!"
  drawText ctx uiLeftCoord (154.0 + 4.0 * charHeight) "Go!  Save your restaurant!"

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
  drawText ctx 0.0 0.0 "Customers"
  traverseWithIndex_ drawCustomer $ evalState (liftCustomerState getCustomers) gs
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

  drawCustomer :: Int -> Customer -> Effect Unit
  drawCustomer ix { order, reward, turnsLeft } = do
    drawText ctx
      0.0
      (toNumber (3 * ix + 1) * charHeight)
      "---"
    drawText ctx
      0.0
      (toNumber (3 * ix + 2) * charHeight)
      ((getItemRecord order).name <> " " <> show turnsLeft)
    drawText ctx
      0.0
      (toNumber (3 * ix + 3) * charHeight)
      ("  " <> displayReward reward)

drawLog :: Context -> GameState -> Effect Unit
drawLog ctx gs = sequence_ $ map drawLogItem (zip (range 1 logLines) (take logLines gs.logevents))
  where
    drawLogItem :: Tuple Int LogEvent -> Effect Unit
    drawLogItem (Tuple index evt) = drawText ctx
      (uiLeftCoord + 150.0)
      (uiTopCoord + (toNumber $ index - 1) * charHeight)
      (logString evt)

    logString :: LogEvent -> String
    logString (ItemEvent item) = "Acquired " <> toLower (itemName item) <> "!"
    logString (CombatEvent mob) = "Hit " <> toLower (mobName mob) <> "!"
    logString (MonsterKilledEvent mob) = "Killed " <> toLower (mobName mob) <> "!"
    logString (PlayerAttacked mob) = "Hit by " <> toLower (mobName mob) <> "!"

logLines :: Int
logLines = 3

uiLeftCoord :: Number
uiLeftCoord = 53.0

uiTopCoord :: Number
uiTopCoord = charHeight

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
