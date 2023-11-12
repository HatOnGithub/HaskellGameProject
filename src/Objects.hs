module Objects where

import Model
import qualified Data.Map as Map
import Graphics.Gloss
import System.Random (mkStdGen, StdGen, Random (random))

-- world

blankWorld :: World
blankWorld = World{
    player        = mario (0,0)
  , enemies       = []
  , blocks        = []
  , pickupObjects = []
  , points        = 0
  , timeLeft      = NA
  , camera        = (0,0)
  , gameState     = Loading
  , worldSize     = (0,0)
  , keyboardState = KeyBoardState []
  , backGround    = Blank
}

-- mario

mario :: Point -> Player
mario pos = Player {
    position      = pos
  , velocity      = (0,0)
  , animations    = Map.empty
  , movementState = Standing
  , powerUpState  = Small
  , boundingBoxS  = (0.9,1)
  , starMan       = False
  , starManTimer  = 0
  , grounded      = False
  , alive         = True
  , isFacingLeft  = False
  , collision     = True
  , iFrames       = True
  }

-- enemies

basicEnemy :: Enemy
basicEnemy = Enemy {
    ename           = ""
  , eposition       = (0,0)
  , evelocity       = (randomSignForFloat mvmntVelocity,0)
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


-- blocks

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

-- pickup objects


basicPickupObject :: PickupObject
basicPickupObject = PickupObject {
    poposition      = (0,0)
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
    poposition  = pos
  , pogravity   = True
  , pickupType  = Mushroom
}

fireFlower :: Point -> PickupObject
fireFlower pos = basicPickupObject {
    poposition  = pos
  , pogravity   = True
  , pickupType  = FireFlower
}

star :: Point -> PickupObject
star pos = basicPickupObject {
    poposition  = pos
  , pogravity   = True
  , bouncy      = True
  , pickupType  = Star
}

--Kiest lekker tussen of een float positief of negatief is
randomSignForFloat :: Float -> Float
randomSignForFloat velo  | dikkeBMW = velo
                      | otherwise = -velo
                where 
                  gen = mkStdGen 351887987986986876
                  dikkeBMW = fst (random gen :: (Bool, StdGen))