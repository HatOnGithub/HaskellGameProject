{-# LANGUAGE NamedFieldPuns #-}
module Controller where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import QuadTree
import Data.List

step :: Float -> World -> IO World
step dt w | gameState w == GoMode = do
            (return . processCollision . processGravity dt . processVectors dt . updateTimes dt) w
          | otherwise             = return w
--step dt = return . updateTimes dt . processCollision . processVectors dt 

input :: Event -> World -> IO World
input (EventKey key Down _ _) w@( World { player, enemies }) = do
    let enemies' = enemies
    if isAlive player then 
        case key of
        (Char 'e') -> return w{enemies = goomba (5,5) : enemies}

        (Char 'w') -> tryJump w
        (Char 'a') -> if movementState player == Crouching then return w{player = player {velocity = velocity player + (-5,  0)}}
            else return w{player = player {velocity = velocity player + (-10,  0)}}
        (Char 's') -> if grounded player then return w{player = player {velocity = (0, snd (velocity player)), movementState = Crouching}} 
                        else return w
        (Char 'd') -> if movementState player == Crouching then return w{player = player {velocity = velocity player + (5,  0)}}
            else return w{player = player {velocity = velocity player + (10,  0)}}
        _ -> return w
    else return w

input (EventKey key Up _ _) w@( World { player }) = do
    let enemies' = enemies w
    if isAlive player then
        case key of
        (Char 'a') ->   if movementState player == Crouching then return w{player = player {velocity = velocity player + (5,  0)}}
            else return w{player = player {velocity = velocity player + (10,  0)}}
        (Char 's') ->   if movementState player == Crouching then return w{player = player {movementState = Standing}} else return w
        (Char 'd') ->   if movementState player == Crouching then return w{player = player {velocity = velocity player + (-5,  0)}}
            else return w{player = player {velocity = velocity player + (-10,  0)}}
        _ -> return w
    else return w
-- unmapped key? unknown input? ignore lmao    
input _ w = return w


tryJump w@( World { player }) | isGrounded player   = return w{player = player {velocity = velocity player + (0, 28), grounded = False, movementState = Jumping}}
                              | otherwise           = return w

collision :: World -> World
collision = undefined

updateTimes :: Float -> World -> World
updateTimes dt w@( World { player, enemies, blocks, pickupObjects, timeLeft }) = w{
      player        = modCurrentAnimation player dt
    , enemies       = map (`modCurrentAnimation` dt) enemies
    , blocks        = map (`modCurrentAnimation` dt) blocks
    , pickupObjects = map (`modCurrentAnimation` dt) pickupObjects
    , timeLeft      = tickTime dt timeLeft
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
applyVectors dt obj = 
    if isAlive obj then 
        setPosition obj (getPosition obj + (getVelocity obj * toPoint dt))
    else 
        obj


applyGravity :: CollisionObject a => Float ->  a ->  a
applyGravity dt obj | not (isAlive obj)          = setVelocity obj (0,0)
                    | not (isGrounded obj)       = setVelocity obj (getVelocity obj + (gravity * toPoint dt))
                    | snd (getVelocity obj) <= 0 = setVelocity obj (fst (getVelocity obj) , -0.1)
                    | otherwise = obj


processCollision ::  World -> World
processCollision w@( World { player, enemies, blocks, pickupObjects, points }) = playerCollision bTree eTree pTree w
    where   bTree = buildQuadTree blocks        (worldSize w)
            eTree = buildQuadTree enemies       (worldSize w)
            pTree = buildQuadTree pickupObjects (worldSize w)

playerCollision :: QuadTree Block -> QuadTree Enemy -> QuadTree PickupObject -> World -> World
playerCollision bTree eTree pTree w@( World { player, enemies, blocks, points, worldSize }) = w {
        player = if snd (getPosition player) >= -5 then np else kill player
    ,   camera = (cx, clamp cy 13.1 (snd worldSize))
    }
    where np        = worldCollision player bTree
          (cx,cy)   = position np + boundingBoxS np * toPoint 0.5

enemyCollision :: World -> World
enemyCollision w@( World { player, enemies, blocks, points }) = w
    where
        eTree = buildQuadTree enemies (worldSize w)
        bTree = buildQuadTree blocks  (worldSize w)

worldCollision :: CollisionObject a => a -> QuadTree Block -> a
worldCollision obj bTree = do
        let possiblePartners    = getCollisionPartners obj bTree
        let collidableBlocks    = filter (obj `collidesWith`) possiblePartners
        let sortedOnDistance    = sortBy (\ a b -> abs (bposition a - getPosition obj) `compare` abs (bposition b - getPosition obj)) collidableBlocks
        let areUnderneath (x,y) = abs x > abs y && y < 0
        let areAbove (x,y)      = abs x > abs y && y > 0
        let isGrounded          = any (areUnderneath . (obj `overlap`)) collidableBlocks
        let hitCeiling          = any (areAbove . (obj `overlap`)) collidableBlocks
        if hitCeiling then groundState (correctPosition (setVelocity obj (getVelocity obj * (1,0))) sortedOnDistance) isGrounded
        else groundState (correctPosition obj sortedOnDistance) isGrounded

correctPosition ::  CollisionObject a => a -> [Block] -> a
correctPosition = foldl (\ obj x -> setPosition obj (getPosition obj - smallestChange (obj `overlap` x)))
    where smallestChange (x,y)  | abs x <  abs y = verySmallOffset * (x , 0)
                                | abs x >= abs y = verySmallOffset * (0 , y)
          verySmallOffset       = (1.001,1.001)

