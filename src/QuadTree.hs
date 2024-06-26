{-# LANGUAGE DeriveFoldable #-}
module QuadTree where

import Model
import Graphics.Gloss

-- QuadTree related stuff


data QuadTree a = Node BoundingBox [a] (QuadTree a) (QuadTree a) (QuadTree a) (QuadTree a) | EmptyLeaf
  deriving (Foldable, Show, Eq)


thresholdObjectsPerQuadrant :: Int
thresholdObjectsPerQuadrant = 6

getCollisionPartners :: (CollisionObject a,  CollisionObject b) => a -> QuadTree b -> [b]
getCollisionPartners _ EmptyLeaf = []
getCollisionPartners obj n@(Node bb objs tl tr bl br )
        -- seems like we reached civilisation
    | not (null objs)   = collapse n
        -- hmm, we can go deeper, no one is here
    | getBB obj `intersects` bb   = concatMap (getCollisionPartners obj) [ tl, tr, bl, br]
        -- huh, there is no one here
    | otherwise         = []

getAllInArea ::  (CollisionObject b) => BoundingBox -> QuadTree b -> [b]
getAllInArea _ EmptyLeaf = []
getAllInArea area n@(Node bb objs tl tr bl br )
    | bb `intersects` area  = filter (\obj -> getBB obj `intersects` area) objs ++ concatMap (getAllInArea area) [tl,tr,bl,br]
    | otherwise             = []

buildQuadTree :: (CollisionObject a, Eq a) => [a] -> Point -> QuadTree a
buildQuadTree l worldSize = insertAll l (Node ((0,0),  worldSize) [] EmptyLeaf EmptyLeaf EmptyLeaf EmptyLeaf)

insertAll :: (CollisionObject a, Eq a) => [a] -> QuadTree a -> QuadTree a
insertAll [] t = t
insertAll (x:xs) t@(Node bb objs tl tr bl br)   = insertAll xs (insert x t)


insert :: (CollisionObject a, Eq a) => a -> QuadTree a -> QuadTree a
insert x t@(Node bb xs tl tr bl br)
    -- doesn't fit, doesn't belong, begone
    | not (x `fitsIn` bb) || not (hasCollision x)= t

    -- there is room and there are no subnodes!, come in!
    | length xs < thresholdObjectsPerQuadrant && not (hasSubNodes t)
        = Node bb (x : xs) tl tr bl br

    -- no more room! we don't have subnodes! subdivide and insert into subnodes!
    | not (hasSubNodes t)
        = insertAll (x:xs) (Node bb [] ntl ntr nbl nbr)

    -- welp, try the subnodes?
    | any (\q -> x `fitsIn` getQTBB q) [ tl, tr, br, bl]
        = insertIntoQuadrant x t

    | otherwise
        = Node bb (x : xs) tl tr bl br

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
    where bb1 = getBB obj1

newNode :: CollisionObject a => BoundingBox -> QuadTree a
newNode bb = Node bb [] EmptyLeaf EmptyLeaf EmptyLeaf EmptyLeaf

hasSubNodes :: (CollisionObject a, Eq a) => QuadTree a -> Bool
hasSubNodes (Node _ _ tl _ _ _) = tl /= EmptyLeaf

getQTBB :: CollisionObject a => QuadTree a -> BoundingBox
getQTBB (Node bb _ _ _ _ _) = bb

collidesWith :: (CollisionObject a, CollisionObject b) => a -> b -> Bool
collidesWith objA objB  | hasCollision objA && hasCollision objB = intersects (getBB objA) (getBB objB)
                        | otherwise                              = False

intersects :: BoundingBox -> BoundingBox -> Bool
intersects ((xA, yA), (wA, hA)) ((xB, yB), (wB, hB)) =
    xA < xB + wB &&
    xA + wA > xB &&
    yA < yB + hB &&
    yA + hA > yB

-- positive x = right; negative x = left
-- positive y = bottom; negative y = top
overlap :: (CollisionObject a, CollisionObject b) => a -> b -> (Float, Float)
overlap objA objB = (xOverlap , yOverlap)
  where ((xA, yA), (wA, hA)) = getBB objA
        ((xB, yB), (wB, hB)) = getBB objB
        xOverlap    | xA <= xB  = (xA + wA) - xB
                    | otherwise = xA - (xB + wB)
        yOverlap    | yA <= yB  = (yA + hA) - yB
                    | otherwise = yA - (yB + hB)
