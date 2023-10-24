module Controller where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Model
import Control.Applicative (Alternative(empty))
import GHC (NoEpAnns)

step :: Float -> World -> IO World
step = undefined

input :: Event -> world -> IO world
input = undefined


buildQuadTree :: CollisionObject a => World -> QuadTree a
buildQuadTree World{ player = p, enemies = es, blocks = bs } = undefined



getPossibleCollisionPartners :: (CollisionObject a, Eq a) => a -> QuadTree a -> [a]
getPossibleCollisionPartners _ EmptyLeaf = []
getPossibleCollisionPartners obj n@(Node bb objs tl tr bl br )  | obj `elem` objs   = collapse n
                                                                | otherwise         = concat [
                                                                    getPossibleCollisionPartners obj tl, 
                                                                    getPossibleCollisionPartners obj tr,
                                                                    getPossibleCollisionPartners obj bl,
                                                                    getPossibleCollisionPartners obj br]

collapse :: CollisionObject a => QuadTree a -> [a]
collapse EmptyLeaf = []
collapse (Node _ objs tl tr bl br) = objs ++ (collapse tl) ++ (collapse tr) ++ (collapse bl) ++ (collapse br)