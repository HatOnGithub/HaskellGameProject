module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game

main :: IO ()
main = playIO (InWindow "MarioFromAliExpress" (512, 420) (0,0))
              black
              60
              initialState
              view
              input
              step