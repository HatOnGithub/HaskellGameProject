{-# LANGUAGE NamedFieldPuns #-}
module Controller where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import QuadTree

step :: Float -> World -> IO World
step dt = return . processCollision . processVectors dt . updateTimes dt

input :: Event -> World -> IO World
input = undefined

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
playerCollision = undefined