{-# LANGUAGE NamedFieldPuns #-}
module Controller where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import QuadTree
import GHC.Plugins (infinity)
import Data.List

step :: Float -> World -> IO World
step dt w | gameState w == GoMode = do
            print (player w)
            (return . processCollision  .   processGravity dt . processVectors dt) w
          | otherwise             = return w
--step dt = return . updateTimes dt . processCollision . processVectors dt 

input :: Event -> World -> IO World
input (EventKey key Down _ _) w@( World { player, enemies }) = do
    let enemies' = enemies
    print (isGrounded player)
    case key of
        (Char 'e') -> return w{enemies = goomba (5,5) : enemies}

        (Char 'w') -> tryJump w
        (Char 'a') -> return w{player = player {velocity = velocity player + (-10,  0)}}
        (Char 's') -> return w{player = player {velocity = velocity player + (  0,-10)}}
        (Char 'd') -> return w{player = player {velocity = velocity player + ( 10,  0)}}

        (SpecialKey KeyUp)      -> return w{camera = camera w + ( 0, 10)}
        (SpecialKey KeyLeft)    -> return w{camera = camera w + (-10, 0)}
        (SpecialKey KeyDown)    -> return w{camera = camera w + ( 0,-10)}
        (SpecialKey KeyRight)   -> return w{camera = camera w + ( 10, 0)}
        _ -> return w
input (EventKey key Up _ _) w@( World { player }) = do
    let enemies' = enemies w
    case key of
        (Char 'a') -> return w{player = player {velocity = velocity player - (-10,  0)}}
        (Char 's') -> return w{player = player {velocity = velocity player - (  0,-10)}}
        (Char 'd') -> return w{player = player {velocity = velocity player - ( 10,  0)}}
        _ -> return w
-- unmapped key? unknown input? ignore lmao    
input _ w = return w


tryJump w@( World { player }) | isGrounded player   = return w{player = player {velocity = velocity player + (0, 30), grounded = False}}
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
applyVectors dt obj = setPosition obj (getPosition obj + (getVelocity obj * toPoint dt))


applyGravity :: CollisionObject a => Float ->  a ->  a
applyGravity dt obj | not (isGrounded obj)       = setVelocity obj (getVelocity obj + (gravity * toPoint dt))
                    | snd (getVelocity obj) <= 0 = setVelocity obj (fst (getVelocity obj) , -0.1)
                    | otherwise = obj


processCollision ::  World -> World
processCollision w@( World { player, enemies, blocks, pickupObjects, points }) = playerCollision bTree eTree pTree w
    where   bTree = buildQuadTree blocks  (worldSize w)
            eTree = buildQuadTree enemies (worldSize w)
            pTree = buildQuadTree pickupObjects (worldSize w)

playerCollision :: QuadTree Block -> QuadTree Enemy -> QuadTree PickupObject -> World -> World
playerCollision bTree eTree pTree w@( World { player, enemies, blocks, points }) = w {player = groundCollision player bTree}

enemyCollision :: World -> World
enemyCollision w@( World { player, enemies, blocks, points }) = w
    where
        eTree = buildQuadTree enemies (worldSize w)
        bTree = buildQuadTree blocks  (worldSize w)

groundCollision :: CollisionObject a => a -> QuadTree Block -> a
groundCollision obj bTree = do
        let possiblePartners    = getCollisionPartners obj bTree
        let collidableBlocks    =  filter (obj `collidesWith`) possiblePartners
        let sortedOnDistance    = sortBy (\ a b -> abs (bposition a - getPosition obj) `compare` abs (bposition b - getPosition obj)) collidableBlocks
        let areUnderneath (x,y) = abs x > abs y && y < 0
        let isGrounded          = any (areUnderneath . (obj `overlap`)) collidableBlocks
        groundState (correctPosition obj sortedOnDistance) isGrounded

correctPosition ::  CollisionObject a => a -> [Block] -> a
correctPosition = foldl (\ obj x -> setPosition obj (getPosition obj - smallestChange (obj `overlap` x)))
    where smallestChange (x,y)  | abs x <  abs y = verySmallOffset * (x , 0)
                                | abs x >= abs y = verySmallOffset * (0 , y)
          verySmallOffset       = (1.001,1.001)

