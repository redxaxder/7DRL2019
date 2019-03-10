module Graphics.Draw where

import Extra.Prelude

import Control.Monad.State (evalState)
import Data.Array (take, zip, range, cons, scanl, (..))
import Data.Enum (enumFromTo)
import Data.Array.NonEmpty as NE
import Data.Foldable (maximum)
import Data.String.CodeUnits (singleton)
import Data.Map (Map, toUnfoldable)
import Data.String.Common (toLower)

import Atlas (Position)
import Constants (canvasDimensions, charHeight, displayDimensions, white, blue, gray)
import Data.Furniture (getFurnitureRecord, FurnitureType)
import Data.Item (getItemRecord)
import Data.Recipe (RecipeRecord)
import Data.Sprite (glitch, player, spriteAt)
import Data.Tile (Tile, tileSprite)
import Graphics.Render
  ( Context
  , drawSpriteToGrid
  , drawTextToGrid
  , drawLinesToGrid
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
  , getUIHints
  )
import Types.Customer (Customer, getCustomers, displayReward)
import Types.Furniture (furnitureSprite)
import Types.Item (itemSprite, itemName)
import Types.Mob (mobSprite, mobName)

draw :: Context -> UIRenderData -> GameState -> Effect Unit
draw ctx uiRenderData gs = do
  clear ctx
  drawMainPane ctx uiRenderData gs
  case uiRenderData of
       StartScreen -> pure unit
       _ -> drawLeftPane ctx uiRenderData gs

-----------------------------------------------------------------
-- ruler for debugging
-----------------------------------------------------------------

drawRuler ctx = drawLinesToGrid ctx white (V {x:0, y:0}) (map show $ enumFromTo 0 29)

-----------------------------------------------------------------
-- Left pane
-----------------------------------------------------------------

drawLeftPane :: Context -> UIRenderData -> GameState -> Effect Unit
drawLeftPane ctx uiRenderData gs = do
  setFillStyle ctx white
  drawStatusInfo ctx gs
  drawSeenInfo ctx gs
  drawLog ctx gs
  drawOrderInfo ctx gs
  drawDividers ctx gs
  drawHints ctx (getUIHints uiRenderData)
  --drawRuler ctx

-- Line 0 to Line 2
drawStatusInfo :: Context -> GameState -> Effect Unit
drawStatusInfo ctx gs = pure unit


-- Line 4 to Line 10
drawSeenInfo :: Context -> GameState -> Effect Unit
drawSeenInfo ctx gs = pure unit

-- Line 12 to line 21
drawLog :: Context -> GameState -> Effect Unit
drawLog ctx gs = drawLinesToGrid ctx white (V {x: 0, y:12}) (logString <$> take 10 gs.logevents)
  where
    logString :: LogEvent -> String
    logString (ItemEvent item) = "Acquired " <> toLower (itemName item) <> "!"
    logString (CombatEvent mob) = "Hit " <> toLower (mobName mob) <> "!"
    logString (MonsterKilledEvent mob) = "Killed " <> toLower (mobName mob) <> "!"
    logString (PlayerAttacked mob) = "Hit by " <> toLower (mobName mob) <> "!"

-- Line 22 to line 24
drawOrderInfo :: Context -> GameState -> Effect Unit
drawOrderInfo ctx gs = pure unit

drawDividers :: Context -> GameState -> Effect Unit
drawDividers _ _ = pure unit

-- Line 27 to line 29
drawHints :: Context -> Array UIHint -> Effect Unit
drawHints ctx hints = drawLinesToGrid ctx white (V {x: 0, y:27}) (assembleUIHint <$> take 3 hints)

-----------------------------------------------------------------
-- Main pane
-----------------------------------------------------------------

drawMainPane :: Context -> UIRenderData -> GameState -> Effect Unit
drawMainPane ctx StartScreen             gs = drawStartScreen ctx
drawMainPane ctx (MainGame _)            gs = drawMainGame ctx gs
drawMainPane ctx (InventoryScreen s _)   gs = drawInventory ctx gs s
drawMainPane ctx (ServeCustomerScreen _) gs = drawServeCustomer ctx gs
drawMainPane ctx (Crafting s r _ f)      gs = drawCrafting ctx gs s r f

drawStartScreen :: Context -> Effect Unit
drawStartScreen ctx = do
  clear ctx
  drawLinesToGrid ctx white (V {x: 16, y: 10})
    [ "You find an interdimensional dungeon"
    , "entrance in the pantry of your"
    , "failing restaurant."
    , "The Monsters are delicious!"
    , "Go!  Save your restaurant!"
    ]

drawMainGame :: Context -> GameState -> Effect Unit
drawMainGame ctx gs = do
  gs.fov # traverse_ \{ screen, tiles } ->
    drawSpriteToGrid ctx (spriteFromTileStack tiles) (toCornerRelative screen)
  drawVisible gs.furniture furnitureSprite
  drawVisible gs.items itemSprite
  drawVisible gs.mobs mobSprite
  drawSpriteToGrid ctx player (toCornerRelative zero)

  where
  drawVisible :: forall a. Map Position a -> (a -> Sprite) -> Effect Unit
  drawVisible m sprite = getVisible gs.fov m # traverse_ \{ a, screen } ->
    drawSpriteToGrid ctx (sprite a) (toCornerRelative screen)

  toCornerRelative :: Vector Int -> Vector Int
  toCornerRelative (V {x,y}) = V { x: x', y: y' }
    where
    x' = x + div displayDimensions.width 2
    y' = y + div displayDimensions.height 2

  spriteFromTileStack :: Array Tile -> Sprite
  spriteFromTileStack xss = case NE.fromArray xss of
    Nothing -> glitch
    Just xs -> tileSprite $ NE.head xs

drawInventory :: Context -> GameState -> (Maybe {label :: Char, item :: Item}) -> Effect Unit
drawInventory ctx gs selected = do
  let getColor = case selected of
        Nothing -> const gray
        Just {label, item} -> \(Tuple c _) -> if (label == c)
                                                then white
                                                else gray
  drawTextToGrid ctx white "Inventory" (V {x: 23, y: 1})
  (toUnfoldable gs.inventory :: Array _) # traverseWithIndex_ \ix item -> do
     drawTextToGrid ctx (getColor item) (itemString item) (V {x: 23, y: 4 + ix})
  where
  itemString (Tuple c i) = (singleton c) <> ") " <> (itemName i)


drawServeCustomer :: Context -> GameState -> Effect Unit
drawServeCustomer ctx gs = drawTODO ctx

drawCrafting
  :: Context -> GameState -> (Array { label :: Char, item :: Item })
  -> Array RecipeRecord -> Maybe FurnitureType -> Effect Unit
drawCrafting ctx gs selectedItems recipes furniture = drawTODO ctx

drawTODO :: Context -> Effect Unit
drawTODO ctx = drawLinesToGrid ctx white (V {x: 20, y: 15}) ["TODO"]

