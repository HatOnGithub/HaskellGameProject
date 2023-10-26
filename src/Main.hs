module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

animLocations :: [(String, String)]
animLocations = []

main :: IO ()
main = do
    
    playIO (InWindow "MarioFromAliExpress" (512, 420) (0,0))
              black
              60
              initialState
              view
              input
              step