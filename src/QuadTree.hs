module QuadTree where

import Model

-- QuadTree related stuff

thresholdObjectsPerQuadrant :: Int
thresholdObjectsPerQuadrant = 6

buildQuadTree :: (CollisionObject a, Eq a) => [a] -> Model.Point -> QuadTree a
buildQuadTree l  worldSize = insertAll l (Node ((0,0),  worldSize) [] EmptyLeaf EmptyLeaf EmptyLeaf EmptyLeaf)

insertAll :: (CollisionObject a, Eq a) => [a] -> QuadTree a -> QuadTree a
insertAll [] t = t
insertAll (x:xs) t@(Node bb objs tl tr bl br)   = insertAll xs (insert x t)


insert :: (CollisionObject a, Eq a) => a -> QuadTree a -> QuadTree a
insert x t@(Node bb xs tl tr bl br) 
    | not (x `fitsIn` bb) = t
    | hasSubNodes t && any (\q -> x `fitsIn` getBB q) [tl, tr, bl, br] 
        = insertIntoQuadrant x t
    | length xs < thresholdObjectsPerQuadrant ||         -- if it fits and there is room OR
      all (\q -> not (x `fitsIn` getBB q)) quadrants     -- the max is reached and it doesn't fit into any subnodes
        = Node bb (x : xs) tl tr bl br                   -- add to layer
    | any (\q -> x `fitsIn` getBB q) quadrants 
        = insertAll (x:xs) (Node bb [] ntl ntr nbl nbr)  -- if the max is reached, attempt subinsert of everything in that layer and move on
    where quadrants@[ntl, ntr, nbl, nbr] = map newNode (toQuadrants bb)

newNode :: CollisionObject a => BoundingBox -> QuadTree a
newNode bb = Node bb [] EmptyLeaf EmptyLeaf EmptyLeaf EmptyLeaf

hasSubNodes :: (CollisionObject a, Eq a) => QuadTree a -> Bool
hasSubNodes (Node _ _ tl tr bl br) = tl /= EmptyLeaf && tr /= EmptyLeaf && bl /= EmptyLeaf && br /= EmptyLeaf

getBB :: CollisionObject a => QuadTree a -> BoundingBox
getBB (Node bb _ _ _ _ _) = bb

insertIntoQuadrant :: (CollisionObject a, Eq a) => a -> QuadTree a -> QuadTree a
insertIntoQuadrant x t@(Node bb objs tl tr bl br) 
    | x `fitsIn` bbOf tl = Node bb objs (insert x tl) tr bl br
    | x `fitsIn` bbOf tr = Node bb objs tl (insert x tr) bl br
    | x `fitsIn` bbOf bl = Node bb objs tl tr (insert x bl) br
    | x `fitsIn` bbOf br = Node bb objs tl tr bl (insert x br)
    where bbOf (Node bb _ _ _ _ _) = bb

toQuadrants :: BoundingBox -> [BoundingBox]
toQuadrants (tl, (sx, sy)) = [(tl, half), (tl `addX` hx, half), (tl `addY` hy, half), (tl `addT` half, half)]
    where half@(hx, hy) = (sx / 2, sy / 2)

getPossibleCollisionPartners :: (CollisionObject a,  CollisionObject b) => a -> QuadTree b -> [b]
getPossibleCollisionPartners _ EmptyLeaf = []
getPossibleCollisionPartners obj n@(Node bb objs tl tr bl br )
    | length objs  >= thresholdObjectsPerQuadrant = collapse n
    | obj `fitsIn` bb   = concat    [ getPossibleCollisionPartners obj tl
                                    , getPossibleCollisionPartners obj tr
                                    , getPossibleCollisionPartners obj bl
                                    , getPossibleCollisionPartners obj br ]
    | otherwise         = []

collapse :: CollisionObject a => QuadTree a -> [a]
collapse EmptyLeaf = []
collapse (Node _ objs tl tr bl br) = concat [objs , collapse tl , collapse tr , collapse bl , collapse br]

fitsIn :: (CollisionObject a) => a -> BoundingBox -> Bool
fitsIn obj1 bb2 = topLeft bb1 >= topLeft bb2 && bottomRight bb1 <= bottomRight bb2
    where bb1 = getBoundingBox obj1

addT :: (Float, Float) -> (Float, Float) -> (Float, Float)
addT (x1, y1) (x2, y2) = (x1 + x2, y1 + y2) 

addX :: (Float, Float) -> Float -> (Float, Float)
addX (x1, y1) x = (x1 + x, y1) 

addY :: (Float, Float) -> Float -> (Float, Float)
addY (x1, y1) y = (x1, y1 + y) 

size :: BoundingBox -> (Float, Float)
size (_,s) = s

bottomRight :: BoundingBox -> (Float, Float)
bottomRight (tl, size) = tl `addT` size

topRight :: BoundingBox -> (Float, Float)
topRight ((x1, y1), (x2, _)) = (x1 + x2, y1) 

bottomLeft :: BoundingBox -> (Float, Float)
bottomLeft ((x1, y1), (_, y2)) = (x1, y1 + y2) 

topLeft :: BoundingBox -> (Float, Float)
topLeft (tl, _) = tl