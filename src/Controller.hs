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
import GHC.Base (neChar)
import WorldLoader (loadLevelAt)
import Objects
import FileLocations

movementKeys :: [Char]
movementKeys =  [ 'w', 'W'
                , 'a', 'A'
                , 's', 'S'
                , 'd', 'D' ]


step :: Map String (Map String Animation) -> Float -> World -> IO World
step m rawdt w  | gameState w == Loading = (return  . assignAnimations m) w {gameState = GoMode}
                | gameState w == GoMode  = (return  . assignAnimations m . stepPure dt) w
                | otherwise              = return w
        where dt = rawdt * worldSpeed

stepPure ::  Float -> World -> World
stepPure dt w | gameState w == GoMode = ( updateTimes dt . cullDeadObjects . processCollision . processGravity dt . processVectors dt . processInputs . updateMovementStates ) w
              | otherwise             =  w

input :: Event -> World -> IO World
input (EventKey (Char key) Down mod _) w@( World { player, enemies, keyboardState = kbs@(KeyBoardState keys) }) = do
    let enemies' = enemies
    if key == 'e' then return w{enemies = goomba (8,4) : enemies}
    else if key `notElem` keys && key `elem` movementKeys then
        if shift mod == Down then return w {keyboardState = kbs { keys = toUpper key : keys}}
        else return w {keyboardState = kbs { keys = toLower key : keys}}
    else return w
input (EventKey (SpecialKey KeyEsc) Down _ _ )  w@(World {player, gameState})   
    | gameState == GoMode && isAlive player = return w {gameState = Pause}
    | gameState == Pause                    = return w {gameState = GoMode}
    | otherwise                             = (loadLevelAt . snd . head) levels
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
  | hasNoAnimations obj && isJust (map !? getName obj ) = setAnimations obj (map ! getName obj)
  | otherwise                    = obj


updateTimes :: Float -> World -> World
updateTimes dt w@( World { player, enemies, blocks, pickupObjects, timeLeft }) = w{
      player        = modCurrentAnimation (updateStarManTime player dt) dt
    , enemies       = map (`modCurrentAnimation` dt) enemies
    , blocks        = map (`modCurrentAnimation` dt) blocks
    , pickupObjects = map (`modCurrentAnimation` dt) pickupObjects
    , timeLeft      = tickTime dt timeLeft
}
    where 
        updateStarManTime p@(Player {starManTimer = 0}) t = p { starMan = False}
        updateStarManTime p@(Player {starManTimer})     t = p { starManTimer  = max 0 (starManTimer - dt)}

processInputs :: World -> World
processInputs w@(World {player, keyboardState = kbs@(KeyBoardState inputs)}) =  w {player = inputResult player inputs}


-- Last In First Out, last inputs get priority over previous held inputs
inputResult :: Player -> [Char] -> Player
inputResult p []      = p {velocity = velocity p * (0,1)}
inputResult p keys@(k:ks)
    | ('s' `elem` keys || 'S' `elem` keys) && isGrounded p = p {velocity = (fst (velocity p), -0.5), movementState = Crouching }
    | k == 'w'|| k == 'W' = tryJump (inputResult p ks)
    | k == 'd'            = addHorizontalVelocity mvmntVelocity (inputResult p ks)
    | k == 'D'            = addHorizontalVelocity (mvmntVelocity * runModifier) (inputResult p ks)
    | k == 'a'            = addHorizontalVelocity (-mvmntVelocity) (inputResult p ks)
    | k == 'A'            = addHorizontalVelocity (-mvmntVelocity * runModifier) (inputResult p ks)
    | otherwise           = p

 where  tryJump player  | isGrounded player && snd (velocity player) <= 0
                            = player {velocity = (fst (velocity player), jumpVelocity), grounded = False, movementState = Jumping}
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
processVectors dt w@( World { player, enemies, blocks, pickupObjects}) = w {
          player  = applyPlayerVectors dt player (movementState player == Crouching)
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

applyPlayerVectors :: Float -> Player -> Bool -> Player
applyPlayerVectors dt p isCrouching
    | not (isAlive p)   = p
    | isCrouching       = setVel (setPos p (getPos p + (getVel p * toPoint dt ))) deceleration
    | otherwise         = setPos p (getPos p + (getVel p * toPoint dt))
    where deceleration = getVel p - (getVel p * toPoint (0.9 * dt))

applyVectors :: CollisionObject a => Float ->  a ->  a
applyVectors dt obj | isAlive obj   = setPos obj (getPos obj + (getVel obj * toPoint dt))
                    | otherwise      = obj


applyGravity :: CollisionObject a => Float ->  a ->  a
applyGravity dt obj | not (hasGravity obj)      = obj
                    | not (isAlive obj)         = setVel obj (0,0)
                    | not (isGrounded obj)      = setVel obj (getVel obj + gravity * toPoint dt)
                    | snd (getVel obj) <= 0     = setVel obj (fst (getVel obj) , -0.1)
                    | otherwise                 = obj

processCollision ::  World -> World
processCollision w@( World { player, enemies, blocks, pickupObjects, points }) =
    (poCollision        bTree .
     enemyCollision     bTree eTree .
     playerCollision    bTree eTree pTree) w
    where   bTree = buildQuadTree blocks        (worldSize w)
            eTree = buildQuadTree enemies       (worldSize w)
            pTree = buildQuadTree pickupObjects (worldSize w)

playerCollision :: QuadTree Block -> QuadTree Enemy -> QuadTree PickupObject -> World -> World
playerCollision bTree eTree pTree w@( World { player, enemies, pickupObjects, blocks, points, worldSize, timeLeft }) = w {
        player  = if snd (getPos player) >= -5 && timeLeft > Secs 0 then foldl applyPickupEffect np pickups else kill player
    ,   camera  = (clamp cx leftOfScreenClamp (fst worldSize - leftOfScreenClamp - 1), clamp cy bottomOfScreenClamp (snd worldSize))
    ,   blocks  = map fst blockPopPairs
    ,   enemies = nEnems
    ,   pickupObjects = killPickups pickupObjects pickups ++ newPOs
    ,   points  = points + (length (filter (isCoin . snd ) pops) * 200) + (length (filter (\po -> pickupType po == Coin) pickups) * 200)
    ,   gameState = checkWinConditions
    }
    where   wcP             = worldCollision bTree player
            (cx,cy)         = getCenter np
            reachableBlocks = getCollisionPartners np bTree
            (piP, pickups)  = itemCollision wcP pTree
            (np, nEnems)    = interactEnemPlayer piP enemies
            blockPopPairs   = map (testPop np reachableBlocks) blocks
            pops            = filter (\pair -> snd pair /= Empty) blockPopPairs
            newPOs          = map contentsToPO (filter (not . isCoin . snd ) pops)
            checkWinConditions | any (\block -> (getName block == "Pole" || getName block == "Flag") && block `collidesWith` player) reachableBlocks = Win
                               | otherwise = GoMode

interactEnemPlayer :: Player -> [Enemy] -> (Player, [Enemy])
interactEnemPlayer pl [] = (pl, [])
interactEnemPlayer pl enems = (nPl, nEnems)
    where   (mPl, nEnems)   = stompEnems pl enems
            nPl             = damagePlayer mPl (filter (collidesWith mPl) (filter isAlive nEnems))

damagePlayer :: Player -> [Enemy] -> Player
damagePlayer pl@(Player {iFrames}) [] = pl {iFrames = False}
damagePlayer pl@(Player {powerUpState, starMan, iFrames}) _ | iFrames || starMan = pl
                                                            | powerUpState == Small = kill pl
                                                            | powerUpState == Large = pl { powerUpState = Small, iFrames = True}
                                                            | powerUpState == Fire = pl { powerUpState = Large, iFrames = True}


stompEnems ::Player -> [Enemy] -> (Player, [Enemy])
stompEnems pl [] = (pl, [])
stompEnems pl@(Player {velocity, grounded, movementState, starMan}) (enem: enems)
    | starMan = (nPl, killIntersecting enem : nEnems)
    | jump = (pl {velocity = (fst velocity , jumpVelocity), grounded = False, movementState = Jumping} , nEnem:nEnems)
    | otherwise = (nPl, nEnem:nEnems)
    where
        (nEnem, jump) = stompCheck pl enem
        (nPl, nEnems) = stompEnems pl enems
        killIntersecting e | pl `collidesWith` e = kill e
                           | otherwise           = e


stompCheck :: Player -> Enemy -> (Enemy, Bool)
stompCheck pl enem  | getBB enem `intersects` ((x + 0.2, y - 0.1), (w - 0.2, 0.08)) && snd (velocity pl) < 0= (kill enem, True)
                    | otherwise = (enem, False)
    where ((x,y), (w, _)) = getBB pl



itemCollision :: Player -> QuadTree PickupObject -> (Player, [PickupObject])
itemCollision p pTree = (foldl playerItemInteract p collidingObjs, collidingObjs)
        where
            posPickupObj = getCollisionPartners p pTree
            collidingObjs = filter (collidesWith p) posPickupObj

playerItemInteract :: Player -> PickupObject -> Player
playerItemInteract pl@(Player {powerUpState, starMan, starManTimer}) po@(PickupObject { pickupType, poalive})
                            | pickupType == Mushroom && powerUpState == Small   = pl {powerUpState = Large}
                            | pickupType == FireFlower                          = pl {powerUpState = Fire}
                            | pickupType == Star                                = pl {starMan = True, starManTimer = 5}
                            | otherwise                                         = pl


killPickups :: [PickupObject] -> [PickupObject] -> [PickupObject]
killPickups allItems []             = allItems
killPickups allItems pickUppedItems = foldl killPickup allItems pickUppedItems

killPickup :: [PickupObject] -> PickupObject -> [PickupObject]
killPickup [] _ = []
killPickup (x:xs) obj   | x == obj = kill x : xs
                        | otherwise = x : killPickup xs obj

applyPickupEffect :: Player -> PickupObject -> Player
applyPickupEffect p po | pickupType po == Mushroom   = sizeUp p
                       | pickupType po == FireFlower = p {powerUpState = Fire}
                       | pickupType po == Star       = p {starMan = True, starManTimer = 20}
                       | otherwise                   = p

sizeUp :: Player -> Player
sizeUp p@(Player {powerUpState = s}) | s == Small = p {powerUpState = Large}
                                     | otherwise  = p

enemyCollision :: QuadTree Block -> QuadTree Enemy -> World -> World
enemyCollision bTree eTree w@( World { player, enemies, blocks, points }) = w {
    enemies =  map (\ e -> collideEnemy e bTree eTree) enemies
    }


collideEnemy :: Enemy -> QuadTree Block -> QuadTree Enemy -> Enemy
collideEnemy enemy bTree eTree =
    enemSpeed bTree eTree
    (worldCollision bTree enemy)

enemSpeed :: QuadTree Block -> QuadTree Enemy -> Enemy -> Enemy
enemSpeed bTree eTree e | not (null collideBlocks && null collideEnemies) = setVel e (getVel e * (-1, 1) )
                        | otherwise = e
    where
        (vX, _) = getVel e
        ((x, y), (w, h)) = getBB e
        collideBox  | vX >= 0 =((x + (w + 0.001), y + 0.01), (0.0011, h - 0.02))
                    | vX < 0 = ((x - 0.0021, y+ 0.01), (0.0011, h - 0.02))
        collideBlocks  = getAllInArea collideBox bTree
        collideEnemies = getAllInArea collideBox eTree

isCoin :: BlockContents -> Bool
isCoin (Coins _) = True
isCoin _         = False

testPop :: Player -> [Block] -> Block -> (Block, BlockContents)
testPop p reachable b   | b `elem` reachable && headCheck p b = popBlock b
                        | otherwise     = (b, Empty)

popBlock :: Block -> (Block, BlockContents)
popBlock b@(Block {item = Empty})     = (b, Empty)
popBlock b@(Block {item = Coins  1})  = (b {item = Empty}, Coins 1)
popBlock b@(Block {item = Coins  x})  = (b {item = Coins (x - 1)}, Coins x)
popBlock b@(Block {item = Object x})  = (b {item = Empty}, Object x)


contentsToPO :: (Block, BlockContents) -> PickupObject
contentsToPO (b, Object t)  | t == Mushroom     = mushroom   (getPos b + (0,1))
                            | t == FireFlower   = fireFlower (getPos b + (0,1))
                            | t == Star         = star       (getPos b + (0,1))

poCollision :: QuadTree Block  -> World -> World
poCollision bTree w@( World { player, enemies, pickupObjects, blocks, points, worldSize, timeLeft }) = w {
    pickupObjects = map (poBounce bTree . worldCollision bTree) pickupObjects
}

poBounce :: QuadTree Block -> PickupObject -> PickupObject
poBounce bTree po   | not (null collideBlocks) && bouncy po = setVel po (jumpVelocity * 0.5, snd (getVel po) )
                    | otherwise = po
    where
        (vX, _) = getVel po
        ((x, y), (w, h)) = getBB po
        collideBox  | vX >= 0 =((x + (w + 0.001), y + 0.01), (0.0011, h - 0.02))
                    | vX < 0 = ((x - 0.0021, y+ 0.01), (0.0011, h - 0.02))
        collideBlocks  = getAllInArea collideBox bTree

worldCollision :: CollisionObject a =>  QuadTree Block -> a -> a
worldCollision bTree obj = do
        let possiblePartners    = getCollisionPartners obj bTree
            collidableBlocks    = filter (obj `collidesWith`) possiblePartners
            sortedOnDistance    = sortBy (\ a b -> abs (bposition a - getTopCenter obj) `compare` abs (bposition b - getTopCenter obj)) collidableBlocks
            correctedObject     = correctPosition obj sortedOnDistance
            isGrounded          = any (groundCheck correctedObject) collidableBlocks
            hitCeiling          = any (headCheck   correctedObject) collidableBlocks
        if hitCeiling && snd (getVel obj) >= 0 then groundState (setVel correctedObject (getVel correctedObject * (1,0))) isGrounded
        else groundState correctedObject isGrounded

-- since all objects are rectangular, use the smallest change in position to correct for a collision, 
-- work through each collision in order of distance
correctPosition ::  CollisionObject a => a -> [Block] -> a
correctPosition = foldl (\ obj x -> setPos obj (getPos obj - smallestChange (obj `overlap` x)))
    where smallestChange (x,y)  | abs x <  abs y = (x , 0)
                                | abs x >= abs y = (0 , y)

groundCheck :: CollisionObject a => a -> Block -> Bool
groundCheck obj b = getBB b `intersects` ((x + 0.02, y - 0.01), (w - 0.04, 0.01))
    where ((x,y), (w, _)) = getBB obj

headCheck :: CollisionObject a => a -> Block -> Bool
headCheck obj b = snd (getVel obj) >= 0 && getBB b `intersects` ((x + 0.02, h + y), (w - 0.04, 0.01))
    where ((x,y), (w, h)) = getBB obj

cullDeadObjects :: World -> World
cullDeadObjects w@(World{enemies, blocks, pickupObjects}) =
    w { enemies = filter isAlive enemies,
        blocks  = filter isAlive blocks ,
        pickupObjects = filter isAlive pickupObjects}