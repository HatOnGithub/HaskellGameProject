{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module WorldLoader where

import Model
import Data.List.Split (splitOn, whenElt)
import Graphics.Gloss
import AnimationLoader (blShift, scaleToWorld)

loadLevelAt :: FilePath -> IO World
loadLevelAt path = do
    text <- readFile path
    let name            = (head . splitOn "." . last . splitOn "\\") path
      -- size : worldType : timeString : world
        (s:t:tstr:w)    = discardCommentsAndEmptyLines (lines text)         
        [width,height]  = map read (splitOn " " s) :: [Int]
        fw              = fromIntegral width
        time            = read tstr :: Float
    rawbg <- loadBMP (bgPath t)
    let bg = scaleToWorld  rawbg
    return (parseWorld (blankWorld {timeLeft = Secs time, worldSize = (fw,fw), backGround = bg }) width height w)

discardCommentsAndEmptyLines :: [String] -> [String]
discardCommentsAndEmptyLines []     = []
discardCommentsAndEmptyLines (x:xs) | startsWith "//" x || null x = discardCommentsAndEmptyLines xs
                                    | otherwise = x : discardCommentsAndEmptyLines xs

startsWith :: String -> String -> Bool
startsWith _ []          = False
startsWith [] _          = True
startsWith (x:xs) (y:ys) | x == y    = startsWith xs ys
                         | otherwise = False

parseWorld :: World -> Int -> Int -> [String] -> World
parseWorld w _ _ [] = w
parseWorld world w 0 [last] = parseString world w 0 last
parseWorld world w h (l:ls) = parseWorld (parseString world w (h - 1) l) 0 (h - 1) ls

parseString :: World -> Int -> Int -> String -> World
parseString w _ _ [] = w
parseString world w h [char, ';']   = parseChar world 1 h char
parseString world w h (c:cs@(n:ns)) | c == 'P'  = parseString (world {blocks = pipe (fromIntegral w, fromIntegral h) (read [n]) : blocks world}) 
                                                  (w + 2) h ns
                                    | otherwise = parseString (parseChar world w h c) 
                                                  (w + 1) h cs
parseString world w h (c:cs)        = parseString (parseChar world w h c) (w + 1) h cs

parseChar   :: World -> Int -> Int -> Char -> World
parseChar w _ _ '.' = w
parseChar world w h c = case c of
    -- player
    'm' -> world {player = mario pos}
    
    -- blocks
    'G' -> world {blocks = grass  pos           : blocks world}
    'D' -> world {blocks = dirt   pos           : blocks world}
    'H' -> world {blocks = block  pos           : blocks world}
    'B' -> world {blocks = brick  pos           : blocks world}
    'Q' -> world {blocks = qBlock pos (Coins 1)  : blocks world}
    -- special blocks
    '1' -> world {blocks = fakeBrick  pos (Coins 10)     : blocks world}
    '2' -> world {blocks = qBlock pos (Object Mushroom) : blocks world}
    '3' -> world {blocks = fakeBrick  pos (Object Star) : blocks world}
    '#' -> world {blocks = castle pos : blocks world}
    '|' -> world {blocks = pole   pos : blocks world}
    '<' -> world {blocks = flag   pos : blocks world}

    -- enemies
    'g' -> world {enemies = goomba pos : enemies world}
    'k' -> world {enemies = koopa  pos : enemies world}

    _ -> world

    where pos = (fromIntegral w , fromIntegral h )


bgPath :: String -> FilePath
bgPath "Overworld"    = "src\\Textures\\Background\\Overworld.bmp"
bgPath "Underground"  = "src\\Textures\\Background\\Underground.bmp"