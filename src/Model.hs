module Model where
import Data.Char (GeneralCategory(PrivateUse))
import GHC.Data.Bitmap (Bitmap)
import GHC.Unit.Module.Graph (isTemplateHaskellOrQQNonBoot)

initialState :: world
initialState = undefined


data QuadTree a = Node BoundingBox [a] (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a) | EmptyLeaf
  deriving (Show, Eq)

data Time = Secs Float | NA
  deriving (Eq)

data GameState = GoMode | PrivateUse
  deriving (Eq)

type Point = (Float, Float)

type Vector = (Float, Float)

data Animation = Animation { frames :: [Bitmap], timer :: Float, index :: Int, loops :: Bool } 

type BoundingBox = (Point, Point)

data Camera = Point
  deriving (Eq)

data MovementState = Standing | Walking | Running | Jumping | Crouching | Firing
  deriving (Eq)

data PowerUpState = Small | Large | Fire | Starman
  deriving (Eq)

data AIPattern = HopChase | Throw | Patrol | RunAway | Bowser 
  deriving (Eq)

data BlockContents = Object PickupObject | Coin | Empty

data PickupType = Mushroom | FireFlower | Star
  deriving (Eq)

data PickupObject = PickupObject {
      poPosition      :: Point
    , poVelocity      :: Vector
    , pickupType      :: PickupType

}

class CollisionObject a where
  getBoundingBox :: a -> BoundingBox




data Player = Player {
      position      :: Point
    , velocity      :: Vector
    , animations    :: [(String, Animation)]
    , movementState :: MovementState
    , powerUpState  :: PowerUpState
    , boundingBox   :: BoundingBox
} 

testPlayer :: Point -> Player
testPlayer pos = Player {position = pos, velocity = (0,0), animations = [], movementState = Standing, powerUpState = Small, boundingBox = (pos, (1,1))}

instance CollisionObject Player where
  getBoundingBox = boundingBox


data Enemy = Enemy {
      eposition     :: Point
    , evelocity     :: Vector
    , eanimations   :: [(String, Animation)]
    , emovementState:: MovementState
    , aIPattern     :: AIPattern
    , eboundingBox  :: BoundingBox
}
goomba :: Point -> [(String, Animation)] -> Enemy
goomba pos anims = Enemy {eposition = pos, evelocity = (0,0), eanimations = anims, emovementState = Standing, aIPattern = Patrol, eboundingBox = (pos, (1,1)) }

instance CollisionObject Enemy where
  getBoundingBox = eboundingBox

instance Show Enemy where
  show e = "Enemy"

instance Eq Enemy where
  (==) e1 e2 = (eposition e1 == eposition e1) && (evelocity e1 == evelocity e2) && (aIPattern e1 == aIPattern e2)

data Block = Block {
      bposition       :: Point
    , item            :: BlockContents
    , texture         :: [(String, Animation)]
    , bboundingBox    :: BoundingBox
}

instance CollisionObject Block where
  getBoundingBox = bboundingBox

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