{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

module Model where
import Data.Map (Map, (!?), size)
import qualified Data.Map as Map
import Graphics.Gloss (Picture, Point, Vector)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Data.Fixed (mod')

screenSize :: (Int, Int)
screenSize = (1024, 840)

screenSizeWorld :: (Float, Float)
screenSizeWorld = (fromIntegral (fst screenSize) / worldScale, fromIntegral (snd screenSize) / worldScale)

worldSpeed :: Float
worldSpeed = 1

gravity :: (Float, Float)
gravity = (0, -60)

pixelsPerUnit :: Int
pixelsPerUnit = 16

mvmntVelocity :: Float
mvmntVelocity = 5

runModifier :: Float
runModifier = 1.5

jumpVelocity :: Float
jumpVelocity = 26

worldScale :: Float
worldScale = 3 * fromIntegral pixelsPerUnit

bottomOfScreenClamp :: Float
bottomOfScreenClamp = fromIntegral (snd screenSize) / (worldScale * 2)

leftOfScreenClamp :: Float
leftOfScreenClamp = fromIntegral (fst screenSize) / (worldScale * 2)

toPoint :: Num a => a -> (a,a)
toPoint n = (n,n)

addPoints :: Int -> World -> World
addPoints n w@(World {points}) = w{points = points + n}

getCenter :: CollisionObject a => a -> Point
getCenter obj = getPos obj + (snd (getBB obj) * toPoint 0.5)

getTopCenter :: CollisionObject a => a -> Point
getTopCenter obj = getPos obj + (snd (getBB obj) * (0.5, 1))

bottomRight :: BoundingBox -> (Float, Float)
bottomRight (tl, size) = tl + size

topRight :: BoundingBox -> (Float, Float)
topRight ((x1, y1), (x2, _)) = (x1 + x2, y1)

bottomLeft :: BoundingBox -> (Float, Float)
bottomLeft ((x1, y1), (_, y2)) = (x1, y1 + y2)

topLeft :: BoundingBox -> (Float, Float)
topLeft (tl, _) = tl

clamp :: (Ord a) => a -> a -> a -> a
clamp val mn mx = min mx (max mn val)

missingTexture :: Picture
missingTexture = Pictures [
    Color magenta   (Polygon [(0,0), (0,0.5), (0.5,0.5), (0.5,0)]),
    Color black     (Polygon [(0.5,0), (0.5,0.5), (1,0.5), (1,0)]),
    Color magenta   (Polygon [(0.5,0.5), (0.5,1), (1,1), (1,0.5)]),
    Color black     (Polygon [(0,0.5), (0,1), (0.5,1), (0.5,0.5)])]

updateAnim :: Float -> String -> Animation -> Animation
updateAnim dt _ a@(Animation {frames, frameLength, timer, index, loops})
  | timer + dt >= frameLength && index < length frames - 1 = a{timer = timer + dt - frameLength, index = index + 1}
  | timer + dt >= frameLength && loops = a{timer = timer + dt - frameLength, index = 0}
  | timer + dt >= frameLength = a{timer = frameLength}
  | otherwise = a{timer = timer + dt}

data Time = Secs Float | NA
  deriving (Eq, Ord)

data GameState = GoMode | Pause | Win
  deriving (Eq)

data Animation = Animation { frames :: [Picture], frameLength :: Float, timer :: Float, index :: Int, loops :: Bool }

type BoundingBox = (Point, Point)

type Camera = Point

data MovementState = Standing | Walking | Running | Jumping | Falling | Crouching | GroundedFiring | MidAirFiring
  deriving (Eq, Show)

data WorldType = Overworld | Underground | Sky
  deriving (Eq, Show)

data PowerUpState = Small | Large | Fire
  deriving (Eq, Show)

data AIPattern = Inactive | HopChase | Throw | Patrol | RunAway | Bowser
  deriving (Eq, Show)

data BlockContents = Object PickupType | Coins Float | Empty
  deriving (Eq)

data PickupType = Coin | Mushroom | FireFlower | Star
  deriving (Eq, Show)



class CollisionObject a where
  getName             :: a -> String
  getBB               :: a -> BoundingBox
  getVel              :: a -> Vector
  getPos              :: a -> Point
  setBBSize           :: a -> Point -> a
  setVel              :: a -> Vector -> a
  setPos              :: a -> Point -> a
  hasNoAnimations     :: a -> Bool
  setAnimations       :: a -> Map String Animation -> a
  getCurrentAnimation :: a -> Maybe Animation
  modCurrentAnimation :: a -> Float -> a
  getInternalState    :: a -> MovementState
  setInternalState    :: a -> MovementState -> a
  groundState         :: a -> Bool -> a
  isGrounded          :: a -> Bool
  isAlive             :: a -> Bool
  kill                :: a -> a
  facingLeft          :: a -> Bool
  faceLeft            :: a -> Bool -> a
  hasGravity          :: a -> Bool
  hasCollision        :: a -> Bool


data Player = Player {
    position      :: Point
  , velocity      :: Vector
  , animations    :: Map String Animation
  , movementState :: MovementState
  , powerUpState  :: PowerUpState
  , boundingBoxS  :: Point
  , starMan       :: Bool
  , starManTimer  :: Float
  , grounded      :: Bool
  , alive         :: Bool
  , isFacingLeft  :: Bool
  , collision  :: Bool
  , iFrames :: Bool
}

mario :: Point -> Player
mario pos = Player {
    position      = pos
  , velocity      = (0,0)
  , animations    = Map.empty
  , movementState = Standing
  , powerUpState  = Fire
  , boundingBoxS  = (0.9,1)
  , starMan       = False
  , starManTimer  = 0
  , grounded      = False
  , alive         = True
  , isFacingLeft  = False
  , collision     = True
  , iFrames       = True
  }

data Enemy = Enemy {
    ename           :: String
  , eposition       :: Point
  , evelocity       :: Vector
  , eanimations     :: Map String Animation
  , emovementState  :: MovementState
  , currentPattern  :: AIPattern
  , aIPattern       :: AIPattern
  , eboundingBoxS   :: Point
  , egrounded       :: Bool
  , ealive          :: Bool
  , efacingLeft     :: Bool
}
-- DO NOT INSTANTIATE AS IS, IS TEMPLATE
basicEnemy :: Enemy
basicEnemy = Enemy {
    ename           = ""
  , eposition       = (0,0)
  , evelocity       = (-mvmntVelocity,0)
  , eanimations     = Map.empty
  , emovementState  = Standing
  , currentPattern  = Inactive
  , aIPattern       = Patrol
  , eboundingBoxS   = (0.9,1)
  , egrounded       = False
  , ealive          = True
  , efacingLeft     = False }

goomba :: Point -> Enemy
goomba pos = basicEnemy { ename = "Goomba" , eposition = pos}

koopa :: Point -> Enemy
koopa pos = basicEnemy { ename = "Koopa" , eposition = pos }

data Block = Block {
    bname           :: String
  , bposition       :: Point
  , item            :: BlockContents
  , textures        :: Map String Animation
  , bboundingBoxS   :: Point
  , exists          :: Bool
, bCollision        :: Bool
  , poppable        :: Bool
}

basicBlock :: Block
basicBlock = Block{
    bname           = ""
  , bposition       = (0,0)
  , item            = Empty
  , textures        = Map.empty
  , bboundingBoxS   = (1,1)
  , exists          = True
  , bCollision      = True
  , poppable        = False
}

stone :: Point -> Block
stone pos = basicBlock {bname = "Stone", bposition = pos}

grass :: Point -> Block
grass pos = basicBlock {bname = "Grass", bposition = pos}

dirt :: Point -> Block
dirt pos = basicBlock {bname = "Dirt", bposition = pos}

brick :: Point -> Block
brick pos  = basicBlock{ bname = "Brick" , bposition = pos, poppable = True}

fakeBrick :: Point -> BlockContents -> Block
fakeBrick pos item = basicBlock{ bname = "FakeBrick" , bposition = pos, item = item, poppable = True}

block :: Point -> Block
block pos = basicBlock {bname = "Block", bposition = pos}

qBlock :: Point -> BlockContents -> Block
qBlock pos item = basicBlock {bname = "QBlock", bposition = pos, item = item , poppable = True }

pipe :: Point -> Int -> Block
pipe pos length = basicBlock  { bname = "Pipe", bposition = pos
                              , bboundingBoxS   = (2,fromIntegral length)}

castle :: Point -> Block
castle pos = basicBlock { bname = "Castle", bposition = pos
                        , bboundingBoxS = (5,5)}

pole :: Point -> Block
pole pos = basicBlock { bname = "Pole", bposition = pos + (0.35, 0)
                      , bboundingBoxS = (0.4, 1) }

flag :: Point -> Block
flag pos = basicBlock { bname = "Flag", bposition = pos + (0.35, 0), bCollision = False}



data PickupObject = PickupObject {
    poname          :: String
  , poposition      :: Point
  , povelocity      :: Vector
  , pickupType      :: PickupType
  , poanimations    :: Map String Animation
  , poboundingBoxS  :: Point
  , pogrounded      :: Bool
  , poalive         :: Bool
  , pogravity       :: Bool
  , bouncy          :: Bool
}

basicPickupObject :: PickupObject
basicPickupObject = PickupObject {
    poname          = ""
  , poposition      = (0,0)
  , povelocity      = (mvmntVelocity * 1.1, 0)
  , pickupType      = Coin
  , poanimations    = Map.empty
  , poboundingBoxS  = (1,1)
  , pogrounded      = False
  , poalive         = True
  , pogravity       = False
  , bouncy          = False
}

mushroom :: Point -> PickupObject
mushroom pos = basicPickupObject {
    poname      = "Mushroom"
  , poposition  = pos
  , pogravity   = True
  , pickupType  = Mushroom
}

fireFlower :: Point -> PickupObject
fireFlower pos = basicPickupObject {
    poname      = "FireFlower"
  , poposition  = pos
  , pogravity   = True
  , pickupType  = FireFlower
}

star :: Point -> PickupObject
star pos = basicPickupObject {
    poname      = "Star"
  , poposition  = pos
  , pogravity   = True
  , bouncy      = True
  , pickupType  = Star
}

data World = World {
    player        :: Player
  , enemies       :: [Enemy]
  , blocks        :: [Block]
  , pickupObjects :: [PickupObject]
  , points        :: Int
  , timeLeft      :: Time
  , camera        :: Camera
  , gameState     :: GameState
  , worldSize     :: Point
  , keyboardState :: KeyBoardState
  , backGround    :: Picture
}

blankWorld :: World
blankWorld = World{
    player        = mario (0,0)
  , enemies       = []
  , blocks        = []
  , pickupObjects = []
  , points        = 0
  , timeLeft      = NA
  , camera        = (0,0)
  , gameState     = GoMode
  , worldSize     = (0,0)
  , keyboardState = KeyBoardState []
  , backGround    = Blank
}

newtype KeyBoardState = KeyBoardState { keys :: [Char] }


-- turn back, ye who wish for thy sanity, for yonder thou'd'st find the land of Instance Declarations



























-- General Instances
instance Num (Float, Float) where
  (+) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
  (-) (x1,y1) (x2,y2) = (x1 - x2, y1 - y2)
  (*) (x1,y1) (x2,y2) = (x1 * x2, y1 * y2)
  negate (x,y) = (-x,-y)
  abs (x,y) = (abs x, abs y)
  fromInteger n = (fromInteger n, fromInteger n)
  signum = undefined

instance Show BlockContents where
  show (Object _) = "Full"
  show (Coins _)   = "Full"
  show Empty      = "Empty"

instance Show Animation where
  show a = show (length (frames a)) ++ " Frames, " ++ show (frameLength a) ++ " second Framelength, Time: " ++ show (timer a) ++ " , Index " ++ show (index a)

instance Show Time where
  show (Secs f) = show (round f)
  show NA       = "NA"

-- Player Instances
instance CollisionObject Player where
  -- trivial stuff
  getName _ = "Mario";  getVel = velocity; getPos = position; isAlive = alive; kill p = p {alive = False}; facingLeft = isFacingLeft; faceLeft p b = p {isFacingLeft = b}
  setInternalState p newState= p {movementState = newState}; groundState p b = p {grounded = b}; isGrounded = grounded; getInternalState = movementState; hasGravity _ = True ; hasCollision = collision
  -- bit more complicated stuff
  getBB p | movementState p /= Crouching && powerUpState p /= Small  = (position p + (0.05, 0), (0.9,2))
          | otherwise = (position p + (0.05, 0), (0.9,1))
  setBBSize p@(Player {boundingBoxS}) newBBSize = p {boundingBoxS = newBBSize}
  setVel p@(Player {velocity}) newVelocity = p {velocity = newVelocity}
  setPos p@(Player {position}) newPos = p {position = newPos}
  hasNoAnimations p = size (animations p) == 0
  setAnimations p m = p{animations = m}
  getCurrentAnimation p@(Player {movementState, animations, powerUpState}) =  animations !? (show movementState ++ show powerUpState)
  modCurrentAnimation p@(Player {movementState, animations, powerUpState}) dt =
    p { animations = Map.adjustWithKey (updateAnim dt) (show movementState ++ show powerUpState) animations }

instance Show Player where
  show p = show (concatMap show (animations p) )

-- Enemy Instances
instance CollisionObject Enemy where
  -- trivial stuff
  getName = ename ; getBB e = (eposition e + (0.05, 0), eboundingBoxS e);getVel = evelocity;getPos = eposition ; isAlive = ealive ; getInternalState = emovementState; hasCollision _ = True
  isGrounded = egrounded; groundState e b = e{egrounded = b}; setInternalState e newState= e {emovementState = newState};  kill e = e {ealive = False}; hasGravity _ = True
  facingLeft = efacingLeft; faceLeft p b = p{efacingLeft = b}
  -- bit more complicated stuff
  setBBSize obj newBBSize = obj {eboundingBoxS = newBBSize}
  setVel obj newVelocity = obj {evelocity = newVelocity}
  setPos obj newPos = obj {eposition = newPos}
  hasNoAnimations e = size (eanimations e) == 0
  setAnimations e m = e{eanimations = m}
  getCurrentAnimation obj@(Enemy {emovementState, eanimations}) =  eanimations !? show emovementState
  modCurrentAnimation obj@(Enemy {emovementState, eanimations}) dt =
    obj { eanimations = Map.adjustWithKey (updateAnim dt) (show emovementState ) eanimations }

instance Show Enemy where
  show e = "Enemy"

instance Eq Enemy where
  (==) e1 e2 = (eposition e1 == eposition e1) && (evelocity e1 == evelocity e2) && (aIPattern e1 == aIPattern e2)

-- Block Instances
instance CollisionObject Block where
  -- trivial stuff
  getName = bname; getBB b = (bposition b, bboundingBoxS b); getVel _ = (0,0) ; getPos = bposition; setVel obj _ = obj; getInternalState _ = Standing
  setInternalState b _ = b; groundState b _ = b ;  isGrounded _ = True ; isAlive = exists ; kill b = b { exists = False }; hasGravity _ = False; hasCollision = bCollision
  facingLeft _ = False; faceLeft b _ = b
  -- bit more complicated stuff
  setBBSize obj@(Block {bboundingBoxS}) newBBSize = obj {bboundingBoxS = newBBSize}
  setPos obj@(Block {bposition}) newPos = obj {bposition = newPos}
  hasNoAnimations b = size (textures b) == 0
  setAnimations b m = b{textures = m}
  getCurrentAnimation obj@(Block {item, textures}) =  textures !? show item
  modCurrentAnimation obj@(Block {item, textures}) dt =
    obj { textures = Map.adjustWithKey (updateAnim dt) (show item) textures }

instance Eq Block where
  (==) b1 b2 = bposition b1 == bposition b2

instance Show Block where
  show b = "Block at" ++ show (bposition b)

-- PickupObject Instances
instance CollisionObject PickupObject where
  -- trivial stuff
  getName = poname; getBB po = (poposition po + (0.05,0), poboundingBoxS po); getVel = povelocity;getPos = poposition; getInternalState _ = Walking; hasCollision _ = True
  isGrounded = pogrounded ; setInternalState po _ = po ;  isAlive = poalive ;  groundState po b = po{pogrounded = b} ; kill po = po {poalive = False}; hasGravity = pogravity 
  facingLeft _ = False; faceLeft po _ = po
  -- bit more complicated stuff
  setBBSize obj@(PickupObject {poboundingBoxS}) newBBSize = obj {poboundingBoxS = newBBSize}
  setVel obj@(PickupObject {povelocity}) newVelocity = obj {povelocity = newVelocity}
  setPos obj@(PickupObject {poposition}) newPos = obj {poposition = newPos}
  hasNoAnimations po = size (poanimations po) == 0
  setAnimations po m = po{poanimations = m}
  getCurrentAnimation obj@(PickupObject {pickupType, poanimations}) =  poanimations !? show pickupType
  modCurrentAnimation obj@(PickupObject {pickupType, poanimations}) dt =
    obj { poanimations = Map.adjustWithKey (updateAnim dt) (show pickupType ) poanimations }

instance Eq PickupObject where
  (==) a b = poposition a == poposition b && povelocity a == povelocity b && pickupType a == pickupType b
