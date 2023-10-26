module Controller where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import QuadTree (buildQuadTree, collapse)

step :: Float -> World -> IO World
step dt = return . physics dt

input :: Event -> World -> IO World
input = undefined

collision :: World -> World
collision = undefined

physics ::  Float -> World -> World
physics dt w    = w {
        player  = playerPhysics dt p enemyTree blockTree, 
        enemies = enemyPhysics  dt p enemyTree blockTree, 
        blocks  = blockPhysics  dt p enemyTree blockTree 
        }
    where 
        p         = player w
        enemyTree = buildQuadTree (enemies w) (worldSize w)
        blockTree = buildQuadTree (blocks  w) (worldSize w)

playerPhysics :: Float -> Player -> QuadTree Enemy -> QuadTree Block -> Player
playerPhysics dt p es bs = applyGravity dt p

enemyPhysics :: Float -> Player -> QuadTree Enemy -> QuadTree Block -> [Enemy]
enemyPhysics dt p es bs = map (applyGravity dt) (collapse es)

blockPhysics :: Float -> Player -> QuadTree Enemy -> QuadTree Block -> [Block]
blockPhysics dt p es bs = collapse bs

applyGravity :: CollisionObject a => Float ->  a ->  a
applyGravity dt obj = obj