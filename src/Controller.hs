{-# LANGUAGE NamedFieldPuns #-}
module Controller where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import QuadTree
import Data.List
import Data.Map (Map, (!?), (!))
import Data.Maybe
import Data.Char (toUpper, toLower)

movementKeys :: [Char]
movementKeys =  [ 'w', 'W'
                , 'a', 'A'
                , 's', 'S'
                , 'd', 'D' ]

step :: Map String (Map String Animation) -> Float -> World -> IO World
step m dt w | gameState w == GoMode = (return  . assignAnimations m . stepPure dt) w
            | otherwise             = return w

stepPure ::  Float -> World -> World
stepPure dt w | gameState w == GoMode = ( updateTimes dt . processCollision . processGravity dt . processVectors dt . processInputs . updateMovementStates ) w
              | otherwise             =  w

input :: Event -> World -> IO World
input (EventKey (Char key) Down mod _) w@( World { player, enemies, keyboardState = kbs@(KeyBoardState keys) }) = do
    let enemies' = enemies
    if key == 'e' then return w{enemies = goomba (10,5) : enemies}
    else if key `notElem` keys && key `elem` movementKeys then
        if shift mod == Down then return w {keyboardState = kbs { keys = toUpper key : keys}}
        else return w {keyboardState = kbs { keys = toLower key : keys}}
    else return w

input (EventKey (SpecialKey KeyShiftL) Down _ _) w@(World {keyboardState = kbs@(KeyBoardState {keys})}) = return w{keyboardState = kbs { keys = map toUpper keys}}
input (EventKey (SpecialKey KeyShiftL) Up _ _)   w@(World {keyboardState = kbs@(KeyBoardState {keys})}) = return w{keyboardState = kbs { keys = map toLower keys}}

input (EventKey (Char key) Up _ _) w@( World { player, keyboardState = kbs@(KeyBoardState keys) }) = do
    return w {keyboardState = kbs { keys = deleteAllInstancesOf key keys}}
-- unmapped key? unknown input? ignore lmao    
input _ w = return w

deleteAllInstancesOf :: Char -> [Char] -> [Char]
deleteAllInstancesOf c cs = delete (toLower c) (delete (toUpper c) cs)


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

processInputs :: World -> World
processInputs w@(World {player, keyboardState = kbs@(KeyBoardState inputs)}) =  w {player = inputResult player inputs}


-- Last In First Out, last inputs get priority over previous held inputs
inputResult :: Player -> [Char] -> Player
inputResult p []      = p {velocity = velocity p * (0,1)}
inputResult p keys@(k:ks)
    | movementState p == Crouching && ('s' `elem` keys || 'S' `elem` keys) = p {velocity = (0,-0.5), grounded = True, movementState = Crouching }
    | k == 'w'|| k == 'W' = tryJump (inputResult p ks)
    | k == 's'|| k == 'S' = tryCrouch (inputResult p ks)
    | k == 'd'            = addHorizontalVelocity mvmntVelocity (inputResult p ks)
    | k == 'D'            = addHorizontalVelocity (mvmntVelocity * 1.5) (inputResult p ks)
    | k == 'a'            = addHorizontalVelocity (-mvmntVelocity) (inputResult p ks)
    | k == 'A'            = addHorizontalVelocity (-mvmntVelocity * 1.5) (inputResult p ks)

 where  tryJump player  | isGrounded player && snd (velocity player) <= 0
                            = player {velocity = (fst (velocity player), jumpVelocity), grounded = False, movementState = Jumping}
                        | otherwise         = player
        tryCrouch player| isGrounded player = player {velocity = (0, -0.5), movementState = Crouching}
                        | otherwise         = player 
        addHorizontalVelocity amount player = player {velocity = velocity player + (amount, 0), movementState = crouchCancelCheck}
        crouchCancelCheck   | movementState p == Crouching = Standing -- cancels out the crouch
                            | otherwise                    = movementState p


updateMovementStates :: World -> World
updateMovementStates w@( World { player, enemies, blocks, pickupObjects }) =
    w { player        = updateMovementState player
      , enemies       = map updateMovementState enemies
      , pickupObjects = map updateMovementState pickupObjects
    }

updateMovementState :: CollisionObject a => a -> a
updateMovementState a = do
    let (x,y) = getVel a
        obj | x < 0 = faceLeft a True
            | x > 0 = faceLeft a False
            | otherwise = a
    if isGrounded obj then
        if       x == 0                                      then setInternalState obj Standing
        else if  x /= 0 && abs x <  mvmntVelocity * 1.5      then setInternalState obj Walking
        else if  x /= 0 && abs x >= mvmntVelocity * 1.5      then setInternalState obj Running
        else obj
    else if getInternalState obj /= MidAirFiring then 
        if y > 0 then setInternalState obj Jumping
        else setInternalState obj Falling
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
processCollision w@( World { player, enemies, blocks, pickupObjects, points }) = (enemyCollision bTree eTree . playerCollision bTree eTree pTree) w
    where   bTree = buildQuadTree blocks        (worldSize w)
            eTree = buildQuadTree enemies       (worldSize w)
            pTree = buildQuadTree pickupObjects (worldSize w)

playerCollision :: QuadTree Block -> QuadTree Enemy -> QuadTree PickupObject -> World -> World
playerCollision bTree eTree pTree w@( World { player, enemies, blocks, points, worldSize }) = w {
        player = if snd (getPos player) >= -5 then np else kill player
    ,   camera = (cx, clamp cy bottomOfScreenClamp (snd worldSize))
    }
    where np        = worldCollision player bTree
          (cx,cy)   = position np + boundingBoxS np * toPoint 0.5

enemyCollision :: QuadTree Block -> QuadTree Enemy -> World -> World
enemyCollision bTree eTree w@( World { player, enemies, blocks, points }) = w {
    enemies =  map (\ e -> collideEnemy e bTree eTree) enemies
    }



collideEnemy :: Enemy -> QuadTree Block -> QuadTree Enemy -> Enemy
collideEnemy enemy bTree eTree = enemSpeed collideBlocks collideEnemies (worldCollision enemy bTree)
    where
        (vX, _) = getVel enemy
        ((x, y), (w, h)) = getBB enemy
        collideBox | vX >= 0 =((x + w, y),(w, h))
                    |vX < 0 = ((x - w, y), (w, h))
        collideBlocks = getAllInArea collideBox bTree
        collideEnemies = getAllInArea collideBox eTree

enemSpeed :: [Block] -> [Enemy] -> Enemy -> Enemy
enemSpeed [] [] enem = enem
enemSpeed _ _ enem = setVel enem (getVel enem * (-1, 1) )



worldCollision :: CollisionObject a => a -> QuadTree Block -> a
worldCollision obj bTree = do
        let possiblePartners    = getCollisionPartners obj bTree
        let collidableBlocks    = filter (obj `collidesWith`) possiblePartners
        let sortedOnDistance    = sortBy (\ a b -> abs (bposition a - getPos obj) `compare` abs (bposition b - getPos obj)) collidableBlocks
        let areUnderneath (x,y) = abs x > abs y && y <= 0
        let areAbove (x,y)      = abs x > abs y && y > 0
        let isGrounded          = any (areUnderneath . (obj `overlap`)) collidableBlocks
        let hitCeiling          = any (areAbove . (obj `overlap`)) collidableBlocks
        if hitCeiling && snd (getVel obj) >= 0 then groundState (correctPosition (setVel obj (getVel obj * (1,0))) sortedOnDistance) isGrounded
        else groundState (correctPosition obj sortedOnDistance) isGrounded

-- since all objects are rectangular, use the smallest change in position to correct for a collision, 
-- work through each collision in order of distance
correctPosition ::  CollisionObject a => a -> [Block] -> a
correctPosition = foldl (\ obj x -> setPos obj (getPos obj - smallestChange (obj `overlap` x)))
    where smallestChange (x,y)  | abs x <  abs y = (x , 0)
                                | abs x >= abs y = (0 , y)

