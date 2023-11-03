module Main where

import Controller
import Model
import View
import AnimationLoader
import WorldLoader
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss (loadBMP)

animLocations :: [(String, String)]
animLocations = [
      ("Mario" , "src\\Textures\\Mario")
    , ("Goomba", "src\\Textures\\Enemies\\Goomba")

    , ("Brick" , "src\\Textures\\Blocks\\Brick" )
    , ("Pipe"  , "src\\Textures\\Blocks\\Pipe"  )
    , ("Block" , "src\\Textures\\Blocks\\Block" )
    , ("Grass" , "src\\Textures\\Blocks\\Grass" )
    , ("Dirt"  , "src\\Textures\\Blocks\\Dirt"  )
    , ("QBlock", "src\\Textures\\Blocks\\QBlock")
    , ("Castle", "src\\Textures\\Blocks\\Castle")
    , ("Pole"  , "src\\Textures\\Blocks\\Pole"  )
    , ("Flag"  , "src\\Textures\\Blocks\\Flag"  )
    , ("FakeBrick" , "src\\Textures\\Blocks\\FakeBrick" )]

bgPath :: String -> FilePath
bgPath "Overworld"    = ""
bgPath "Underground"  = ""

levels :: [(String, String)]
levels = [("World 1-1", "src\\Worlds\\1-1.txt")]

main :: IO ()
main = do
    anims <- animLocationToMap animLocations
    world <- loadLevelAt (snd ( head levels))
    playIO (InWindow "PringleMan dies from Heart Disease" screenSize (0,0))
            (makeColorI 73 116 235 255)
            120
            world
            view
            input
            (step anims)

