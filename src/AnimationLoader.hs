module AnimationLoader where

import System.Directory.Recursive
import Graphics.Gloss.Interface.IO.Game
import Data.Map hiding (drop, map, splitAt)
import Graphics.Gloss
import qualified Data.Map as Map hiding (drop, map)
import Data.List (elemIndex)
import Data.List.Split
import Data.Maybe
import Model
import Data.Graph (path)

animLocationToMap :: [(String, String)] -> IO (Map String (Map String Animation))
animLocationToMap [] = return Map.empty
animLocationToMap ((name, folderpath):xs) = do
    filePaths <- getFilesRecursive folderpath
    animsInFolder <- loadAnimations filePaths
    tail <- animLocationToMap xs
    return (Map.insert name (Map.fromList animsInFolder) tail)

loadAnimations :: [FilePath] -> IO [(String, Animation)]
loadAnimations []     = return []
loadAnimations (x:xs) = do
    head <- loadAnimation x
    tail <- loadAnimations xs
    return (head : tail)

loadAnimation :: FilePath -> IO (String, Animation)
loadAnimation path = do
    let stripDir = last (splitOn "\\" path)
        [name,details] = splitOn "_" stripDir
        [strsheetDimensions, frameTime, looping] = splitOn "," details
        sheetDimensions@[x,y] = map read (splitOn "x" strsheetDimensions)
    frames <- loadSpriteSheet path (x,y)
    let anim = Animation {
        frames = frames,
        frameLength = read frameTime,
        timer = 0,
        index = 0,
        loops = case looping of
            "T.bmp" -> True
            "F.bmp" -> False
            _   -> False
    }
    return (name, anim)

loadSpriteSheet :: FilePath -> (Int, Int) -> IO [Picture]
loadSpriteSheet path ssDimensions= do
    wholeSheet@(Bitmap bitmapData ) <- loadBMP path
    let bmpDimensions = bitmapSize bitmapData
        pics = slice ssDimensions bmpDimensions wholeSheet
    return pics

slice :: (Int, Int) -> (Int, Int) -> Picture -> [Picture]
slice sheetDimensions@(rows, columns) bmpDimensions image =
    map (scaleToWorld . blShift ) ( concatMap ( sliceColumns columns) (sliceRows rows image))

sliceRows :: Int -> Picture -> [Picture]
sliceRows rs (Bitmap bitmapData) = map (\cNr -> bitmapSection (Rectangle (0, s * cNr) (w, s)) bitmapData) [0 .. rs - 1]
    where   (w,h) = bitmapSize bitmapData
            s     = h `div` rs

sliceColumns :: Int  -> Picture -> [Picture]
sliceColumns cs (BitmapSection (Rectangle (x,y) (w,h)) bmpD) = map (\cNr -> bitmapSection (Rectangle (x + s * cNr, y) (s, h)) bmpD) [0 .. cs - 1]
    where s       = w `div` cs

blShift :: Picture -> Picture
blShift bms@(BitmapSection (Rectangle (x,y) (w,h)) bmpD) =
    Translate (abs (fromIntegral (w - pixelsPerUnit)) / 2) (abs (fromIntegral (h - pixelsPerUnit)) / 2) bms

scaleToWorld :: Picture -> Picture
scaleToWorld = Translate 0.5 0.5 . Scale (1/fromIntegral pixelsPerUnit) (1/fromIntegral pixelsPerUnit)

