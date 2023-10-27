{-# LANGUAGE NamedFieldPuns #-}
module Controller where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import QuadTree

step :: Float -> World -> IO World
step dt = return 
--step dt = return . processCollision . processVectors dt . updateTimes dt

input :: Event -> World -> IO World
input (EventKey key Down _ _) w = do
    let enemies' = enemies w
    case key of
        (Char 'e') -> return w{enemies = goomba (5,5) : enemies w}
        (Char 'w') -> return w{enemies = map (\enemy -> enemy{eposition = eposition enemy + ( 0, 5)}) enemies'}
        (Char 'a') -> return w{enemies = map (\enemy -> enemy{eposition = eposition enemy + (-5, 0)}) enemies'}
        (Char 's') -> return w{enemies = map (\enemy -> enemy{eposition = eposition enemy + ( 0,-5)}) enemies'}
        (Char 'd') -> return w{enemies = map (\enemy -> enemy{eposition = eposition enemy + ( 5, 0)}) enemies'}
        (SpecialKey KeyUp)      -> return w{camera = camera w + ( 0, 1)}
        (SpecialKey KeyLeft)    -> return w{camera = camera w + (-1, 0)}
        (SpecialKey KeyDown)    -> return w{camera = camera w + ( 0,-1)}
        (SpecialKey KeyRight)   -> return w{camera = camera w + ( 1, 0)}
        _ -> return w
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
          player  = applyGravityAndVectors dt player
        , enemies = map (applyGravityAndVectors dt) enemies
        , blocks  = map (applyGravityAndVectors dt) blocks
        }

applyGravityAndVectors :: CollisionObject a => Float ->  a ->  a
applyGravityAndVectors dt obj = 
    setVelocity 
    (setPosition obj (getPosition obj + ((getVelocity obj + gravity )* toPoint (dt * movementModifier)))) 
    (getVelocity obj + gravity * toPoint dt)

processCollision ::  World -> World
processCollision w@( World { player, enemies, blocks, points }) = w
    where 
        enemyTree = buildQuadTree enemies (worldSize w)
        blockTree = buildQuadTree blocks  (worldSize w)

playerCollision :: World -> World
playerCollision w = w