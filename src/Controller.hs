{-# LANGUAGE NamedFieldPuns #-}
module Controller where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import QuadTree
import Data.List
import Data.Map (Map, (!?), (!))
import Data.Maybe


step :: Map String (Map String Animation) -> Float -> World -> IO World
step m dt w | gameState w == GoMode = (return  . assignAnimations m . stepPure dt) w
            | otherwise             = return w
--step dt = return . updateTimes dt . processCollision . processVectors dt 

stepPure ::  Float -> World -> World
stepPure dt w | gameState w == GoMode = (updateTimes dt . processCollision . processGravity dt . processVectors dt . updateMovementStates) w
              | otherwise             =  w

input :: Event -> World -> IO World
input (EventKey key Down _ _) w@( World { player, enemies }) = do
    let enemies' = enemies
    if isAlive player then
        case key of
        (Char 'e') -> return w{enemies = goomba (5,5) : enemies}

        (Char 'w') -> tryJump w
        (Char 'a') -> if movementState player == Crouching then return w{player = player {velocity = velocity player + (-(mvmntVelocity / 2),  0)}}
            else return w{player = player {velocity = velocity player + (-mvmntVelocity,  0)}}
        (Char 's') -> if grounded player then return w{player = player {velocity = (0, snd (velocity player)), movementState = Crouching}}
                        else return w
        (Char 'd') -> if movementState player == Crouching then return w{player = player {velocity = velocity player + (mvmntVelocity / 2,  0)}}
            else return w{player = player {velocity = velocity player + (mvmntVelocity,  0)}}
        _ -> return w
    else return w

input (EventKey key Up _ _) w@( World { player }) = do
    let enemies' = enemies w
    if isAlive player then
        case key of
        (Char 'a') ->   if movementState player == Crouching then return w{player = player {velocity = velocity player + (mvmntVelocity / 2,  0)}}
            else return w{player = player {velocity = velocity player + (mvmntVelocity,  0)}}
        (Char 's') ->   if movementState player == Crouching then return w{player = player {movementState = Standing}} else return w
        (Char 'd') ->   if movementState player == Crouching then return w{player = player {velocity = velocity player + (-(mvmntVelocity / 2),  0)}}
            else return w{player = player {velocity = velocity player + (-mvmntVelocity,  0)}}
        _ -> return w
    else return w
-- unmapped key? unknown input? ignore lmao    
input _ w = return w

tryJump w@( World { player }) | isGrounded player   = return w{player = player {velocity = velocity player + (0, jumpVelocity), grounded = False, movementState = Jumping}}
                              | otherwise           = return w


-- all pure stuff here

assignAnimations :: Map String (Map String Animation) -> World -> World
assignAnimations m w@(World {
    player, enemies, blocks, pickupObjects, points,
    timeLeft, camera, gameState, worldSize}) = do
        let newP  = tryAssign m player
            newEs = map (tryAssign m) enemies
            newBs = map (tryAssign m) blocks
            newPos= map (tryAssign m) pickupObjects
        w {player = newP, enemies = newEs, blocks = newBs, pickupObjects = newPos}

tryAssign :: CollisionObject a => Map String (Map String Animation) -> a -> a
tryAssign map obj
  | not    (hasNoAnimations obj) = obj
  | isJust (map !? getName obj ) = setAnimations obj (map ! getName obj)
  | otherwise                    = obj

updateTimes :: Float -> World -> World
updateTimes dt w@( World { player, enemies, blocks, pickupObjects, timeLeft }) = w{
      player        = modCurrentAnimation player dt
    , enemies       = map (`modCurrentAnimation` dt) enemies
    , blocks        = map (`modCurrentAnimation` dt) blocks
    , pickupObjects = map (`modCurrentAnimation` dt) pickupObjects
    , timeLeft      = tickTime dt timeLeft
}

updateMovementStates :: World -> World
updateMovementStates w@( World { player, enemies, blocks, pickupObjects }) =
    w{
        player = updateMovementState player
    }

updateMovementState :: CollisionObject a => a -> a
updateMovementState a = do
    let (x,y) = getVel a
        obj | x < 0 = faceLeft a True
            | x > 0 = faceLeft a False
            | otherwise = a
    if isGrounded obj then
        if getInternalState obj == Crouching                 then obj
        else if  x == 0                                      then setInternalState obj Standing
        else if  x /= 0 && abs x <  mvmntVelocity * 1.5      then setInternalState obj Walking
        else if  x /= 0 && abs x >= mvmntVelocity * 1.5      then setInternalState obj Running
        else obj
    else if getInternalState obj /= MidAirFiring  then setInternalState obj Jumping
    else obj



tickTime :: Float -> Time -> Time
tickTime _ NA = NA
tickTime dt (Secs n) = Secs (n - dt)


processVectors ::  Float -> World -> World
processVectors dt w@( World { player, enemies, blocks, pickupObjects }) = w {
          player  = applyVectors dt player
        , enemies = map (applyVectors dt) enemies
        , blocks  = map (applyVectors dt) blocks
        , pickupObjects = map (applyVectors dt) pickupObjects
        }

processGravity :: Float -> World -> World
processGravity dt w@( World { player, enemies, pickupObjects }) = w {
          player  = applyGravity dt player
        , enemies = map (applyGravity dt) enemies
        , pickupObjects = map (applyGravity dt) pickupObjects
        }

applyVectors :: CollisionObject a => Float ->  a ->  a
applyVectors dt obj =
    if isAlive obj then
        setPos obj (getPos obj + getVel obj * toPoint dt)
    else
        obj


applyGravity :: CollisionObject a => Float ->  a ->  a
applyGravity dt obj | not (isAlive obj)          = setVel obj (0,0)
                    | not (isGrounded obj)       = setVel obj (getVel obj + gravity * toPoint dt)
                    | snd (getVel obj) <= 0 = setVel obj (fst (getVel obj) , -0.1)
                    | otherwise = obj

processCollision ::  World -> World
processCollision w@( World { player, enemies, blocks, pickupObjects, points }) = playerCollision bTree eTree pTree w
    where   bTree = buildQuadTree blocks        (worldSize w)
            eTree = buildQuadTree enemies       (worldSize w)
            pTree = buildQuadTree pickupObjects (worldSize w)

playerCollision :: QuadTree Block -> QuadTree Enemy -> QuadTree PickupObject -> World -> World
playerCollision bTree eTree pTree w@( World { player, enemies, blocks, points, worldSize }) = w {
        player = if snd (getPos player) >= -5 then np else kill player
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
        let sortedOnDistance    = sortBy (\ a b -> abs (bposition a - getPos obj) `compare` abs (bposition b - getPos obj)) collidableBlocks
        let areUnderneath (x,y) = abs x > abs y && y <= 0
        let areAbove (x,y)      = abs x > abs y && y > 0
        let isGrounded          = any (areUnderneath . (obj `overlap`)) collidableBlocks
        let hitCeiling          = any (areAbove . (obj `overlap`)) collidableBlocks
        if hitCeiling then groundState (correctPosition (setVel obj (getVel obj * (1,0))) sortedOnDistance) isGrounded
        else groundState (correctPosition obj sortedOnDistance) isGrounded

-- since all objects are rectangular, use the smallest change in position to correct for a collision, 
-- work through each collision in order of distance
correctPosition ::  CollisionObject a => a -> [Block] -> a
correctPosition = foldl (\ obj x -> setPos obj (getPos obj - smallestChange (obj `overlap` x)))
    where smallestChange (x,y)  | abs x <  abs y = (x , 0)
                                | abs x >= abs y = (0 , y)

