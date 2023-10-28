{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
module Model where
import Data.Char (GeneralCategory(PrivateUse))
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import GHC.Data.Bitmap (Bitmap)
import GHC.Unit.Module.Graph (isTemplateHaskellOrQQNonBoot)
import Graphics.Gloss (Picture, Point, Vector)

initialState :: World
initialState = World{ 
  player = testPlayer (10,2.8), 
  enemies = [], 
  blocks = [newBlock (10,2), newBlock (11,2),newBlock (9,2),newBlock (12,2),newBlock (12,3)], 
  pickupObjects = [],
  timeLeft = NA, 
  points = 0, 
  camera = (10,10), 
  gameState = GoMode, 
  worldSize = (100,100)}

movementModifier :: Float
movementModifier = 1

gravity :: (Float, Float)
gravity = (0, -60)

instance Num (Float, Float) where
  (+) (x1,y1) (x2,y2) = (x1 + x2, y1 + y2)
  (-) (x1,y1) (x2,y2) = (x1 - x2, y1 - y2)
  (*) (x1,y1) (x2,y2) = (x1 * x2, y1 * y2)
  negate (x,y) = (-x,-y)
  abs (x,y) = (abs x, abs y)
  fromInteger n = (fromInteger n, fromInteger n)
  signum = undefined

toPoint :: Num a => a -> (a,a)
toPoint n = (n,n)



data QuadTree a = Node BoundingBox [a] (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a) | EmptyLeaf
  deriving (Show, Eq)

data Time = Secs Float | NA
  deriving (Eq)

data GameState = GoMode | Pause
  deriving (Eq)

data Animation = Animation { frames :: [Picture], frameLength :: Float, timer :: Float, index :: Int, loops :: Bool }

type BoundingBox = (Point, Point)

type Camera = Point

data MovementState = Standing | Walking | Running | Jumping | Crouching | GroundedFiring | MidAirFiring
  deriving (Eq, Show)

data PowerUpState = Small | Large | Fire | Starman Float
  deriving (Eq, Show)

data AIPattern = HopChase | Throw | Patrol | RunAway | Bowser
  deriving (Eq, Show)

data BlockContents = Object PickupObject | Coin | Empty

instance Show BlockContents where
  show (Object _) = "Full"
  show Coin       = "Full"
  show Empty      = "Empty"


data PickupType = Mushroom | FireFlower | Star
  deriving (Eq, Show)



class CollisionObject a where
  getBoundingBox      :: a -> BoundingBox
  getVelocity         :: a -> Vector
  getPosition         :: a -> Point
  setBoundingBoxSize  :: a -> Point -> a
  setVelocity         :: a -> Vector -> a
  setPosition         :: a -> Point -> a
  getCurrentAnimation :: a -> Maybe Animation
  modCurrentAnimation :: a -> Float -> a
  setInternalState    :: a -> MovementState -> a
  groundState         :: a -> Bool -> a
  isGrounded          :: a -> Bool


data Player = Player {
      position      :: Point
    , velocity      :: Vector
    , animations    :: Map String Animation
    , movementState :: MovementState
    , powerUpState  :: PowerUpState
    , boundingBoxS  :: Point
    , starMan       :: Bool
    , grounded      :: Bool
    , alive         :: Bool
}

testPlayer :: Point -> Player
testPlayer pos = Player {
  position = pos, 
  velocity = (0,0), 
  animations = Map.empty, 
  movementState = Standing, 
  powerUpState = Small, 
  boundingBoxS = (1,1), 
  starMan = False,
  grounded = False,
  alive = True
  }

instance CollisionObject Player where
  getBoundingBox p = (position p, boundingBoxS p) 
  getVelocity = velocity
  getPosition = position
  setBoundingBoxSize p@(Player {boundingBoxS}) newBBSize = p {boundingBoxS = newBBSize}
  setVelocity p@(Player {velocity}) newVelocity = p {velocity = newVelocity}
  setPosition p@(Player {position}) newPos = p {position = newPos}
  getCurrentAnimation p@(Player {movementState, animations, powerUpState}) =  animations !? (show movementState ++ show powerUpState)
  modCurrentAnimation p@(Player {movementState, animations, powerUpState}) dt =  
    p { animations = Map.adjustWithKey (updateAnim dt) (show movementState ++ show powerUpState) animations }
  setInternalState p newState= p {movementState = newState}
  groundState p b = p {grounded = b}
  isGrounded = grounded 




instance Show Player where
  show p = "Player At " ++ Prelude.show (position p) ++ " with Velocity " ++ Prelude.show (velocity p) ++ " and is Grounded:" ++ Prelude.show (grounded p)

data Enemy = Enemy {
      eposition     :: Point
    , evelocity     :: Vector
    , eanimations   :: Map String Animation
    , emovementState:: MovementState
    , aIPattern     :: AIPattern
    , eboundingBoxS  :: Point
    , egrounded     :: Bool
}

goomba :: Point -> Enemy
goomba pos = Enemy {
  eposition = pos, 
  evelocity = (0,0), 
  eanimations = Map.empty, 
  emovementState = Standing, 
  aIPattern = Patrol, 
  eboundingBoxS = (1,1), 
  egrounded = False }

instance CollisionObject Enemy where
  getBoundingBox e = (eposition e, eboundingBoxS e)
  getVelocity = evelocity
  getPosition = eposition
  setBoundingBoxSize obj@(Enemy {eboundingBoxS}) newBBSize = obj {eboundingBoxS = newBBSize}
  setVelocity obj@(Enemy {evelocity}) newVelocity = obj {evelocity = newVelocity}
  setPosition obj@(Enemy {eposition}) newPos = obj {eposition = newPos}
  getCurrentAnimation obj@(Enemy {emovementState, eanimations}) =  eanimations !? show emovementState
  modCurrentAnimation obj@(Enemy {emovementState, eanimations}) dt =  
    obj { eanimations = Map.adjustWithKey (updateAnim dt) (show emovementState ) eanimations }
  setInternalState e newState= e {emovementState = newState}
  groundState e b = e{egrounded = b}
  isGrounded = egrounded

instance Show Enemy where
  show e = "Enemy"

instance Eq Enemy where
  (==) e1 e2 = (eposition e1 == eposition e1) && (evelocity e1 == evelocity e2) && (aIPattern e1 == aIPattern e2)

data Block = Block {
      bposition       :: Point
    , item            :: BlockContents
    , textures        :: Map String Animation
    , bboundingBoxS    :: Point
}

newBlock :: Point -> Block
newBlock pos = Block{
  bposition = pos,
  item = Empty,
  textures = Map.empty,
  bboundingBoxS = (1,1)
}

popBlock :: Block -> Block
popBlock b | show (item b) == "Full" = b{item = Empty}
           | otherwise = b

instance CollisionObject Block where
  getBoundingBox b = (bposition b, bboundingBoxS b)
  getVelocity _ = (0,0)
  getPosition   = bposition
  setBoundingBoxSize obj@(Block {bboundingBoxS}) newBBSize = obj {bboundingBoxS = newBBSize}
  setVelocity obj _ = obj
  setPosition obj@(Block {bposition}) newPos = obj {bposition = newPos}
  getCurrentAnimation obj@(Block {item, textures}) =  textures !? show item
  modCurrentAnimation obj@(Block {item, textures}) dt =  
    obj { textures = Map.adjustWithKey (updateAnim dt) (show item) textures }
  setInternalState b _ = b
  groundState b _ = b
  isGrounded _ = True

instance Eq Block where
  (==) b1 b2 = bposition b1 == bposition b2

instance Show Block where
  show b = "Block at" ++ show (bposition b)

data PickupObject = PickupObject {
      poposition      :: Point
    , povelocity      :: Vector
    , pickupType      :: PickupType
    , poanimations    :: Map String Animation
    , poboundingBoxS  :: Point
    , pogrounded      :: Bool
} 

instance CollisionObject PickupObject where
  getBoundingBox po = (poposition po, poboundingBoxS po)
  getVelocity = povelocity
  getPosition = poposition
  setBoundingBoxSize obj@(PickupObject {poboundingBoxS}) newBBSize = obj {poboundingBoxS = newBBSize}
  setVelocity obj@(PickupObject {povelocity}) newVelocity = obj {povelocity = newVelocity}
  setPosition obj@(PickupObject {poposition}) newPos = obj {poposition = newPos}
  getCurrentAnimation obj@(PickupObject {pickupType, poanimations}) =  poanimations !? show pickupType
  modCurrentAnimation obj@(PickupObject {pickupType, poanimations}) dt =  
    obj { poanimations = Map.adjustWithKey (updateAnim dt) (show pickupType ) poanimations }
  setInternalState po _ = po 
  groundState po b = po{pogrounded = b}
  isGrounded = pogrounded

instance Eq PickupObject where
  (==) a b = poposition a == poposition b && povelocity a == povelocity b && pickupType a == pickupType b

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