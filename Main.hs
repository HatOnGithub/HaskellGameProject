module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import Language.Haskell.TH (viewP)

main :: IO ()
main = playIO (InWindow "MarioByAliExpress" (512, 420) (0,0))
              black
              60
              initialState
              view
              input
              step