module QuadTree where

import Model
import Graphics.Gloss

-- QuadTree related stuff

thresholdObjectsPerQuadrant :: Int
thresholdObjectsPerQuadrant = 6

getCollisionPartners :: (CollisionObject a,  CollisionObject b) => a -> QuadTree b -> [b]
getCollisionPartners _ EmptyLeaf = []
getCollisionPartners obj n@(Node bb objs tl tr bl br )
        -- seems like we reached non subdevided objects
    | not (null objs)   = collapse n 
        -- hmm, we can go deeper, ain't nothing here
    | obj `fitsIn` bb   = concatMap (getCollisionPartners obj) [ tl, tr, bl, br] 
        -- uh, where is everyone?
    | otherwise         = []

buildQuadTree :: (CollisionObject a, Eq a) => [a] -> Point -> QuadTree a
buildQuadTree l worldSize = insertAll l (Node ((0,0),  worldSize) [] EmptyLeaf EmptyLeaf EmptyLeaf EmptyLeaf)

insertAll :: (CollisionObject a, Eq a) => [a] -> QuadTree a -> QuadTree a
insertAll [] t = t
insertAll (x:xs) t@(Node bb objs tl tr bl br)   = insertAll xs (insert x t)


insert :: (CollisionObject a, Eq a) => a -> QuadTree a -> QuadTree a
insert x t@(Node bb xs tl tr bl br)
    -- doesn't fit, doesn't belong, begone
    | not (x `fitsIn` bb) = t

    -- there is room and there are no subnodes!, come in!
    | length xs < thresholdObjectsPerQuadrant && not (hasSubNodes t) 
        = Node bb (x : xs) tl tr bl br

    -- no more room! we don't have subnodes! subdivide and insert into subnodes!
    | not (hasSubNodes t)
        = insertAll (x:xs) (Node bb [] ntl ntr nbl nbr)

    -- welp, try the subnodes?
    | otherwise
        = insertIntoQuadrant x t 

    where quadrants@[ntl, ntr, nbl, nbr] = map newNode (toQuadrants bb)

insertIntoQuadrant :: (CollisionObject a, Eq a) => a -> QuadTree a -> QuadTree a
insertIntoQuadrant x t@(Node bb objs tl tr bl br)
    | x `fitsIn` bbOf tl = Node bb objs (insert x tl) tr bl br
    | x `fitsIn` bbOf tr = Node bb objs tl (insert x tr) bl br
    | x `fitsIn` bbOf bl = Node bb objs tl tr (insert x bl) br
    | x `fitsIn` bbOf br = Node bb objs tl tr bl (insert x br)
    where bbOf (Node bb _ _ _ _ _) = bb

toQuadrants :: BoundingBox -> [BoundingBox]
toQuadrants (tl, (sx, sy)) = [(tl, half), (tl + (hx, 0.0), half), (tl + (0.0 , hy), half), (tl + half, half)]
    where half@(hx, hy) = (sx / 2, sy / 2)

collapse :: CollisionObject a => QuadTree a -> [a]
collapse EmptyLeaf = []
collapse (Node _ objs tl tr bl br) = concat [objs , collapse tl , collapse tr , collapse bl , collapse br]

fitsIn :: (CollisionObject a) => a -> BoundingBox -> Bool
fitsIn obj1 bb2 = topLeft bb1 >= topLeft bb2 && bottomRight bb1 <= bottomRight bb2
    where bb1 = getBoundingBox obj1

newNode :: CollisionObject a => BoundingBox -> QuadTree a
newNode bb = Node bb [] EmptyLeaf EmptyLeaf EmptyLeaf EmptyLeaf

hasSubNodes :: (CollisionObject a, Eq a) => QuadTree a -> Bool
hasSubNodes (Node _ _ tl _ _ _) = tl /= EmptyLeaf

getBB :: CollisionObject a => QuadTree a -> BoundingBox
getBB (Node bb _ _ _ _ _) = bb

collidesWith :: (CollisionObject a, CollisionObject b) => a -> b -> Bool
collidesWith objA objB = 
    xA < xB + wB &&
    xA + wA > xB &&
    yA < yB + hB &&
    yA + hA > yB
    where   ((xA, yA), (wA, hA)) = getBoundingBox objA
            ((xB, yB), (wB, hB)) = getBoundingBox objB

-- positive x = right; negative x = left
-- positive y = bottom; negative y = top
overlap :: (CollisionObject a, CollisionObject b) => a -> b -> (Float, Float)
overlap objA objB = (xOverlap , yOverlap)
  where ((xA, yA), (wA, hA)) = getBoundingBox objA
        ((xB, yB), (wB, hB)) = getBoundingBox objB
        xOverlap    | xA <= xB  = (xA + wA) - xB
                    | otherwise = xA - (xB + wB)
        yOverlap    | yA <= yB  = (yA + hA) - yB
                    | otherwise = yA - (yB + hB)
