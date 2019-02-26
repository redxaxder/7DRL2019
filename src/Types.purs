module Types where


import Atlas (Atlas, Position)
import Tile (Tile)

type GameState =
 { player :: Position
 , atlas :: Atlas Tile
 }


data UIRenderData = MainGame | StartScreen
