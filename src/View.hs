{-# LANGUAGE NamedFieldPuns #-}
module View where
import Graphics.Gloss
import Model
import Data.Map hiding (map)
import qualified Data.Map as Map
import Data.Maybe (isJust)
import Prelude hiding (flip)
import QuadTree

-- oooooh scary IO
view ::  World -> IO Picture
view  w = do
    let pics = reverse (viewPure w)
    return (Pictures pics)


-- safe pure stuff
viewPure :: World -> [Picture]
viewPure w = viewUI w ++ viewWorld w

viewUI :: World -> [Picture]
viewUI w@(World {
    player, enemies, blocks, pickupObjects, points,
    timeLeft, camera, gameState, worldSize}) =
        if isAlive player then [Blank] else [Translate (-120) 5 (Scale 0.5 0.5 (Text "Skill Issue"))]

viewWorld :: World -> [Picture]
viewWorld w@(World {
    player, enemies, blocks, pickupObjects, points,
    timeLeft, camera, gameState, worldSize}) =
        getFrame camera player  :
        Prelude.map (getFrame camera) enemies ++
        Prelude.map (getFrame camera) blocks ++
        Prelude.map (getFrame camera) pickupObjects 
        -- ++[viewQT w]

getFrame :: CollisionObject a => Camera -> a -> Picture
getFrame c obj = 
    if isAlive obj then 
        if facingLeft obj then flip (getFrame' obj c (getCurrentAnimation obj))
        else getFrame' obj c (getCurrentAnimation obj) 
    else Blank

flip :: Picture -> Picture
flip = Translate 0.5 0 . Scale (-1) 1

getFrame' :: CollisionObject a => a -> Camera -> Maybe Animation -> Picture
getFrame' obj c (Just a) = Scale worldScale worldScale (uncurry Translate (getPos obj - c) (frames a !! index a))
getFrame' obj c Nothing  = Scale worldScale worldScale (uncurry Translate (getPos obj - c) (Scale x y missingTexture))
    where (_, (x,y)) = getBB obj


-- no longer used, took too much effort to throw away


viewQT :: World -> Picture
viewQT w@(World {
    player, enemies, blocks, pickupObjects, points,
    timeLeft, camera, gameState, worldSize}) = Pictures [
        quadTreeToPictures (0.5 `withAlpha` red  ) camera bTree,
        quadTreeToPictures (0.5 `withAlpha` blue ) camera eTree,
        quadTreeToPictures (0.5 `withAlpha` green) camera poTree]
    where
        eTree  = buildQuadTree enemies       worldSize
        bTree  = buildQuadTree blocks        worldSize
        poTree = buildQuadTree pickupObjects worldSize

quadTreeToPictures :: CollisionObject a => Color -> Point -> QuadTree a -> Picture
quadTreeToPictures _ _ EmptyLeaf = Blank
quadTreeToPictures color cam (Node (pos,(w,h)) _ tl tr bl br) = Pictures [
    Color color (Scale worldScale worldScale (uncurry Translate (pos - cam) (Line [(0,0), ( w, 0), (w, h), (0, h), (0,0)]))),
    quadTreeToPictures (dim color) cam tl,
    quadTreeToPictures (dim color) cam tr,
    quadTreeToPictures (dim color) cam bl,
    quadTreeToPictures (dim color) cam br]

