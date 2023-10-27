{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleInstances #-}
module Model where
import Data.Char (GeneralCategory(PrivateUse))
import Data.Map (Map, (!?))
import qualified Data.Map as Map
import GHC.Data.Bitmap (Bitmap)
import GHC.Unit.Module.Graph (isTemplateHaskellOrQQNonBoot)
import Graphics.Gloss (Picture, Point, Vector)
import ImageLoader

initialState :: World
initialState = World{ player = testPlayer (0,0), enemies = [], blocks = [], timeLeft = NA, points = 0, camera = (0,0), gameState = Pause, worldSize = (0,0)}

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

data Animation = Animation { frames :: [Picture], timer :: Float, index :: Int, loops :: Bool }

type BoundingBox = (Point, Point)

type Camera = Point

data MovementState = Standing | Walking | Running | Jumping | Crouching | GroundedFiring | MidAirFiring
  deriving (Eq, Show)

isGrounded :: MovementState -> Bool
isGrounded state = state /= Jumping && state /= MidAirFiring

data PowerUpState = Small | Large | Fire | Starman Float
  deriving (Eq, Show)

data AIPattern = HopChase | Throw | Patrol | RunAway | Bowser
  deriving (Eq, Show)

data BlockContents = Object PickupObject | Coin | Empty
  deriving (Show, Eq)

data PickupType = Mushroom | FireFlower | Star
  deriving (Eq, Show)

data PickupObject = PickupObject {
      poPosition      :: Point
    , poVelocity      :: Vector
    , pickupType      :: PickupType
} deriving (Show, Eq)

class CollisionObject a where
  getBoundingBox :: a -> BoundingBox
  getVelocity :: a -> Vector
  getPosition :: a -> Point
  setBoundingBox :: a -> BoundingBox -> a
  setVelocity :: a -> Vector -> a
  setPosition :: a -> Point -> a
  getCurrentAnimation :: a -> Maybe Animation


data Player = Player {
      position      :: Point
    , velocity      :: Vector
    , animations    :: Map String Animation
    , movementState :: MovementState
    , powerUpState  :: PowerUpState
    , boundingBox   :: BoundingBox
}

testPlayer :: Point -> Player
testPlayer pos = Player {position = pos, velocity = (0,0), animations = Map.empty, movementState = Standing, powerUpState = Small, boundingBox = (pos, (1,1))}

instance CollisionObject Player where
  getBoundingBox = boundingBox
  getVelocity = velocity
  getPosition = position
  setBoundingBox obj@(Player {boundingBox}) newBB = obj {boundingBox = newBB}
  setVelocity obj@(Player {velocity}) newVelocity = obj {velocity = newVelocity}
  setPosition obj@(Player {position}) newPos = obj {position = newPos}
  getCurrentAnimation obj@(Player {movementState, animations, powerUpState}) =  animations !? (show movementState ++ show powerUpState)

instance Show Player where
  show p = "Player At " ++ Prelude.show (position p) ++ " with Velocity " ++ Prelude.show (velocity p)

data Enemy = Enemy {
      eposition     :: Point
    , evelocity     :: Vector
    , eanimations   :: Map String Animation
    , emovementState:: MovementState
    , aIPattern     :: AIPattern
    , eboundingBox  :: BoundingBox
}

goomba :: Point -> Enemy
goomba pos = Enemy {eposition = pos, evelocity = (0,0), eanimations = Map.empty, emovementState = Standing, aIPattern = Patrol, eboundingBox = (pos, (1,1)) }

instance CollisionObject Enemy where
  getBoundingBox = eboundingBox
  getVelocity = evelocity
  getPosition = eposition
  setBoundingBox obj@(Enemy {eboundingBox}) newBB = obj {eboundingBox = newBB}
  setVelocity obj@(Enemy {evelocity}) newVelocity = obj {evelocity = newVelocity}
  setPosition obj@(Enemy {eposition}) newPos = obj {eposition = newPos}
  getCurrentAnimation obj@(Enemy {emovementState, eanimations}) =  eanimations !? show emovementState

instance Show Enemy where
  show e = "Enemy"

instance Eq Enemy where
  (==) e1 e2 = (eposition e1 == eposition e1) && (evelocity e1 == evelocity e2) && (aIPattern e1 == aIPattern e2)

data Block = Block {
      bposition       :: Point
    , item            :: BlockContents
    , textures         :: Map String Animation
    , bboundingBox    :: BoundingBox
}

instance CollisionObject Block where
  getBoundingBox = bboundingBox
  getVelocity _ = (0,0)
  getPosition   = bposition
  setBoundingBox obj@(Block {bboundingBox}) newBB = obj {bboundingBox = newBB}
  setVelocity obj _ = obj
  setPosition obj@(Block {bposition}) newPos = obj {bposition = newPos}
  getCurrentAnimation obj@(Block {item, textures}) =  textures !? show item

instance Eq Block where
  (==) b1 b2 = bposition b1 == bposition b2

data World = World {
      player        :: Player
    , enemies       :: [Enemy]
    , blocks        :: [Block]
    , points        :: Int
    , timeLeft      :: Time
    , camera        :: Camera
    , gameState     :: GameState
    , worldSize     :: Point
    }


bottomRight :: BoundingBox -> (Float, Float)
bottomRight (tl, size) = tl + size

topRight :: BoundingBox -> (Float, Float)
topRight ((x1, y1), (x2, _)) = (x1 + x2, y1)

bottomLeft :: BoundingBox -> (Float, Float)
bottomLeft ((x1, y1), (_, y2)) = (x1, y1 + y2)

topLeft :: BoundingBox -> (Float, Float)
topLeft (tl, _) = tl