module Main where

import Controller
import Model
import View
import AnimationLoader
import WorldLoader
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss (loadBMP)
import FileLocations


main :: IO ()
main = do
    anims <- animLocationToMap animLocations
    world <- loadLevelAt (snd ( head levels))
    playIO (InWindow "PringleMan dies from Heart Disease" screenSize (448,120))
            (makeColorI 73 116 235 255)
            120
            world
            view
            input
            (step anims)

