module Main where

import Controller
import Model
import View
import System.Directory

import Graphics.Gloss.Interface.IO.Game
import Data.Map hiding (drop, map)
import Graphics.Gloss (loadBMP)
import qualified Data.Map as Map hiding (drop, map)
import Data.List (elemIndex)
import Data.Maybe

animLocations :: [(String, String)]
animLocations = []

main :: IO ()
main = do
    anims <- loadAnimationsInAll animLocations
    playIO (InWindow "MarioFromAliExpress" (1024, 840) (0,0))
              blue
              120
              initialState
              (view anims)
              input
              step

loadAnimationsInAll :: [(String, String)] -> IO (Map String (Map String Animation))
loadAnimationsInAll list = do
    raw <- mapM loadAnimations list
    return (Map.fromList raw)

loadAnimations :: (String,String) -> IO (String, Map String Animation)
loadAnimations (name , location) = do
    contentLocations <- getDirectoryContents location
    loneAnims <- mapM toAnimation contentLocations
    let contents = Map.fromList (giveNames contentLocations loneAnims)
    return (name,  contents)

toAnimation :: String -> IO Animation
toAnimation location = undefined

giveNames :: [String] -> [Animation] -> [(String, Animation)]
giveNames (n:ns) (a:as) = (stripPath n, a) : giveNames ns as
    where stripPath string  | isJust (elemIndex '/' n) = stripPath (drop (fromJust (elemIndex '/' string)) string)
                            | otherwise                = string

