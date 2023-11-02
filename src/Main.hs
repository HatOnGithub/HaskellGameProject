module Main where

import Controller
import Model
import View
import AnimationLoader
import Graphics.Gloss.Interface.IO.Game

animLocations :: [(String, String)]
animLocations = [
      ("Mario", "src\\Textures\\Mario")
    , ("Brick", "src\\Textures\\Blocks\\Brick")]


levels :: [(String, String)]
levels = []

main :: IO ()
main = do
    anims <- animLocationToMap animLocations
    playIO (InWindow "PringleMan dies from Heart Disease" (1024, 840) (0,0))
            blue
            120
            initialState
            view
            input
            (step anims)

