{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}

module Model where
import Data.Char (GeneralCategory(PrivateUse))
import Data.Map (Map, (!?), size)
import qualified Data.Map as Map
import Graphics.Gloss (Picture, Point, Vector)
import Graphics.Gloss.Data.Picture
import Graphics.Gloss.Data.Color
import Data.Fixed (mod')

initialState :: World
initialState = World{
  player = mario (11,2.8),
  enemies = [],
  blocks = [brick (7,2), brick (8,2), brick (9,2), brick (10,2), brick (11,2),brick (9,2), brick (10,3)
           ,brick (12,2),brick (12,3),brick (13,2),brick (14,2),brick (15,2),brick (12,7)],
  pickupObjects = [],
  timeLeft = NA,
  points = 0,
  camera = (10,10),
  gameState = GoMode,
  worldSize = (32,32),
  keyboardState = KeyBoardState []}

missingTexture :: Picture
missingTexture = Pictures [
    Color magenta   (Polygon [(0,0), (0,0.5), (0.5,0.5), (0.5,0)]),
    Color black     (Polygon [(0.5,0), (0.5,0.5), (1,0.5), (1,0)]),
    Color magenta   (Polygon [(0.5,0.5), (0.5,1), (1,1), (1,0.5)]),
    Color black     (Polygon [(0,0.5), (0,1), (0.5,1), (0.5,0.5)])]

movementModifier :: Float
movementModifier = 1

worldSpeed :: Float
worldSpeed = 1

gravity :: (Float, Float)
gravity = (0, -60)

pixelsPerUnit :: Int
pixelsPerUnit = 16

mvmntVelocity :: Float
mvmntVelocity = 10

jumpVelocity :: Float
jumpVelocity = 26

worldScale :: Float
-- zoom * sprite size
worldScale = 3 * fromIntegral pixelsPerUnit

bottomOfScreenClamp :: Float
bottomOfScreenClamp = 6.55

toPoint :: Num a => a -> (a,a)
toPoint n = (n,n)

data Time = Secs Float | NA
  deriving (Eq)

data GameState = GoMode | Pause
  deriving (Eq)

data Animation = Animation { frames :: [Picture], frameLength :: Float, timer :: Float, index :: Int, loops :: Bool }

type BoundingBox = (Point, Point)

type Camera = Point

data MovementState = Standing | Walking | Running | Jumping | Falling | Crouching | GroundedFiring | MidAirFiring
  deriving (Eq, Show)

data PowerUpState = Small | Large | Fire
  deriving (Eq, Show)

data AIPattern = HopChase | Throw | Patrol | RunAway | Bowser
  deriving (Eq, Show)

data BlockContents = Object PickupObject | Coin | Empty


data PickupType = Mushroom | FireFlower | Star
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
  }

data Enemy = Enemy {
    ename           :: String
  , eposition       :: Point
  , evelocity       :: Vector
  , eanimations     :: Map String Animation
  , emovementState  :: MovementState
  , aIPattern       :: AIPattern
  , eboundingBoxS   :: Point
  , egrounded       :: Bool
  , ealive          :: Bool
  , efacingLeft     :: Bool
}

goomba :: Point -> Enemy
goomba pos = Enemy {
    ename           = "Goomba"
  , eposition       = pos
  , evelocity       = (7,0)
  , eanimations     = Map.empty
  , emovementState  = Standing
  , aIPattern       = Patrol
  , eboundingBoxS   = (0.9,1)
  , egrounded       = False
  , ealive          = True
  , efacingLeft     = False }

data Block = Block {
    bname           :: String
  , bposition       :: Point
  , item            :: BlockContents
  , textures        :: Map String Animation
  , bboundingBoxS   :: Point
  , exists          :: Bool
}

brick :: Point -> Block
brick pos = Block{
    bname           = "Brick"
  , bposition       = pos
  , item            = Empty
  , textures        = Map.empty
  , bboundingBoxS   = (1,1)
  , exists          = True
}

popBlock :: Block -> (Block, BlockContents)
popBlock b | show (item b) == "Full" = (b{item = Empty}, item b)
           | otherwise = (b, Empty)

data PickupObject = PickupObject {
    poname          :: String
  , poposition      :: Point
  , povelocity      :: Vector
  , pickupType      :: PickupType
  , poanimations    :: Map String Animation
  , poboundingBoxS  :: Point
  , pogrounded      :: Bool
  , poalive         :: Bool
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
}

newtype KeyBoardState = KeyBoardState {
   keys         :: [Char]
}

addPoints :: Int -> World -> World
addPoints n w@(World {points}) = w{points = points + n}

updateAnim :: Float -> String -> Animation -> Animation
updateAnim dt _ a@(Animation {frames, frameLength, timer, index, loops})
  | timer + dt >= frameLength && index < length frames - 1 = a{timer = timer + dt - frameLength, index = index + 1}
  | timer + dt >= frameLength && loops = a{timer = timer + dt - frameLength, index = 0}
  | timer + dt >= frameLength = a{timer = frameLength}
  | otherwise = a{timer = timer + dt}

bottomRight :: BoundingBox -> (Float, Float)
bottomRight (tl, size) = tl + size

topRight :: BoundingBox -> (Float, Float)
topRight ((x1, y1), (x2, _)) = (x1 + x2, y1)

bottomLeft :: BoundingBox -> (Float, Float)
bottomLeft ((x1, y1), (_, y2)) = (x1, y1 + y2)

topLeft :: BoundingBox -> (Float, Float)
topLeft (tl, _) = tl

clamp :: (Ord a) => a -> a -> a -> a
clamp val minimum maximum = min maximum (max minimum val)

-- for your own sanity, don't look down here

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
  show Coin       = "Full"
  show Empty      = "Empty"

instance Show Animation where
  show a = show (length (frames a)) ++ " Frames, " ++ show (frameLength a) ++ " second Framelength, Time: " ++ show (timer a) ++ " , Index " ++ show (index a)

-- Player Instances
instance CollisionObject Player where
  -- trivial stuff
  getName _ = "Mario";  getVel = velocity; getPos = position; isAlive = alive; kill p = p {alive = False}; facingLeft = isFacingLeft; faceLeft p b = p {isFacingLeft = b}
  setInternalState p newState= p {movementState = newState}; groundState p b = p {grounded = b}; isGrounded = grounded; getInternalState = movementState
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
  getName = ename ; getBB e = (eposition e + (0.05, 0), eboundingBoxS e);getVel = evelocity;getPos = eposition ; isAlive = ealive ; getInternalState = emovementState
  isGrounded = egrounded; groundState e b = e{egrounded = b}; setInternalState e newState= e {emovementState = newState};  kill e = e {ealive = False}
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
  setInternalState b _ = b; groundState b _ = b ;  isGrounded _ = True ; isAlive = exists ; kill b = b { exists = False }
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
  getName = poname; getBB po = (poposition po + (0.05,0), poboundingBoxS po); getVel = povelocity;getPos = poposition; getInternalState _ = Walking
  isGrounded = pogrounded ; setInternalState po _ = po ;  isAlive = poalive ;  groundState po b = po{pogrounded = b} ; kill po = po {poalive = False}
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
