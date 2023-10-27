{-# LANGUAGE NamedFieldPuns #-}
module Controller where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import QuadTree

step :: Float -> World -> IO World
step dt = return . processVectors dt 
--step dt = return . processCollision . processVectors dt . updateTimes dt

input :: Event -> World -> IO World
input (EventKey key Down _ _) w@( World { player, enemies }) = do
    let enemies' = enemies
    case key of
        (Char 'e') -> return w{enemies = goomba (5,5) : enemies}

        (Char 'w') -> return w{player = player {velocity = velocity player + (  0, 15)}}
        (Char 'a') -> return w{player = player {velocity = velocity player + (-15,  0)}}
        (Char 's') -> return w{player = player {velocity = velocity player + (  0,-15)}}
        (Char 'd') -> return w{player = player {velocity = velocity player + ( 15,  0)}}

        (SpecialKey KeyUp)      -> return w{camera = camera w + ( 0, 10)}
        (SpecialKey KeyLeft)    -> return w{camera = camera w + (-10, 0)}
        (SpecialKey KeyDown)    -> return w{camera = camera w + ( 0,-10)}
        (SpecialKey KeyRight)   -> return w{camera = camera w + ( 10, 0)}
        _ -> return w
input (EventKey key Up _ _) w@( World { player }) = do
    let enemies' = enemies w
    case key of
        (Char 'w') -> return w{player = player {velocity = velocity player - (  0, 15)}}
        (Char 'a') -> return w{player = player {velocity = velocity player - (-15,  0)}}
        (Char 's') -> return w{player = player {velocity = velocity player - (  0,-15)}}
        (Char 'd') -> return w{player = player {velocity = velocity player - ( 15,  0)}}
        _ -> return w

-- unmapped key? unknown input? ignore lmao    
input _ w = return w

collision :: World -> World
collision = undefined

updateTimes :: Float -> World -> World
updateTimes dt w@( World { timeLeft }) = w{
    timeLeft = tickTime dt timeLeft
}

tickTime :: Float -> Time -> Time
tickTime _ NA = NA
tickTime dt (Secs n) = Secs (n - dt)
    

processVectors ::  Float -> World -> World
processVectors dt w@( World { player, enemies, blocks }) = w {
          player  = applyVectors dt player
        , enemies = map (applyVectors dt) enemies
        , blocks  = map (applyVectors dt) blocks
        }

processGravity :: Float -> World -> World
processGravity dt w@( World { player, enemies, blocks }) = w {
          player  = applyGravity dt player
        , enemies = map (applyGravity dt) enemies
        }


applyVectors :: CollisionObject a => Float ->  a ->  a
applyVectors dt obj = setPosition obj (getPosition obj + (getVelocity obj * toPoint dt))

applyGravity :: CollisionObject a => Float ->  a ->  a
applyGravity dt obj = setPosition obj (getPosition obj + (gravity * toPoint dt))

processCollision ::  World -> World
processCollision w@( World { player, enemies, blocks, points }) = w
    where 
        enemyTree = buildQuadTree enemies (worldSize w)
        blockTree = buildQuadTree blocks  (worldSize w)

playerCollision :: World -> World
playerCollision w = w