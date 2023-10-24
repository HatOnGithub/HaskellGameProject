module Model where
import Data.Char (GeneralCategory(PrivateUse))
import GHC.Data.Bitmap (Bitmap)
import GHC.Unit.Module.Graph (isTemplateHaskellOrQQNonBoot)

initialState :: world
initialState = undefined

thresholdObjectsPerQuadrant :: Int
thresholdObjectsPerQuadrant = 6

data QuadTree a = Node BoundingBox [a] (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a) | EmptyLeaf

data Time = Secs Float | NA

data GameState = GoMode | PrivateUse

type Point = (Float, Float)

type Vector = (Float, Float)

data Animation = Animation { frames :: [Bitmap], timer :: Float, index :: Int, loops :: Bool }

type BoundingBox = ((Float, Float), (Float, Float))

data Camera = Point

data MovementState = Standing | Walking | Running | Jumping | Crouching | Firing

data PowerUpState = Small | Large | Fire | Starman

data AIPattern = HopChase | Throw | Patrol | RunAway | Bowser

data BlockContents = Object PickupObject | Coin | Empty

data PickupType = Mushroom | FireFlower | Star

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

instance CollisionObject Player where
  getBoundingBox = boundingBox


data Enemy = Enemy {
      eposition     :: Point
    , evelocity     :: Vector
    , eanimations   :: [(String, Animation)]
    , aIPattern     :: AIPattern
    , eboundingBox  :: BoundingBox
}

instance CollisionObject Enemy where
  getBoundingBox = eboundingBox

data Block = Block {
      bposition       :: Point
    , item            :: BlockContents
    , texture         :: [(String, Animation)]
    , bboundingBox    :: BoundingBox
}

instance CollisionObject Block where
  getBoundingBox = bboundingBox

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