module Main where

import Controller
import Model
import View
import AnimationLoader
import WorldLoader
import Graphics.Gloss.Interface.IO.Game

animLocations :: [(String, String)]
animLocations = [
      ("Mario", "src\\Textures\\Mario")
    , ("Brick", "src\\Textures\\Blocks\\Brick")]


levels :: [(String, String)]
levels = [("World 1-1", "src\\Worlds\\1-1.txt")]

main :: IO ()
main = do
    anims <- animLocationToMap animLocations
    world <- loadLevelAt (snd ( head levels))
    playIO (InWindow "PringleMan dies from Heart Disease" (1024, 840) (0,0))
            blue
            120
            world
            view
            input
            (step anims)

