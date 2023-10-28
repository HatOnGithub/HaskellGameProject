{-# LANGUAGE NamedFieldPuns #-}
module View where
import Graphics.Gloss
import Model
import GHC.Data.Bitmap
import Data.Map hiding (map)
import GHC.Driver.Session (positionIndependent)
import QuadTree
import GHC.Stg.Syntax (AltType(PolyAlt))


worldScale :: Float
worldScale = 4 * 8


view :: Map String (Map String Animation) -> World -> IO Picture
view map w = do 
    let pics = reverse (viewPure map w)
    return (Pictures pics)

viewPure :: Map String (Map String Animation) -> World -> [Picture]
viewPure map w@(World {
    player, enemies, blocks, pickupObjects, points,
    timeLeft, camera, gameState, worldSize}) = 
        viewUI map w ++ viewWorld map w



viewUI :: Map String (Map String Animation) -> World -> [Picture]
viewUI map w@(World {
    player, enemies, blocks, pickupObjects, points,
    timeLeft, camera, gameState, worldSize}) = [Blank]

viewWorld :: Map String (Map String Animation) -> World -> [Picture]
viewWorld map w@(World {
    player, enemies, blocks, pickupObjects, points,
    timeLeft, camera, gameState, worldSize}) =         
        getFrame camera player  : 
        Prelude.map (getFrame camera) enemies ++ 
        Prelude.map (getFrame camera) blocks ++ 
        Prelude.map (getFrame camera) pickupObjects ++ 
        [viewQT w]

getFrame :: CollisionObject a => Camera -> a -> Picture
getFrame c obj = getFrame' obj c (getCurrentAnimation obj)

getFrame' :: CollisionObject a => a -> Camera -> Maybe Animation -> Picture
getFrame' obj c (Just a) = Scale worldScale worldScale (uncurry Translate (getPosition obj - c) (frames a !! index a))
getFrame' obj c Nothing  = Scale worldScale worldScale (uncurry Translate (getPosition obj - c) missingTexture)

viewQT :: World -> Picture
viewQT w@(World {
    player, enemies, blocks, pickupObjects, points,
    timeLeft, camera, gameState, worldSize}) = Pictures [toPictures red camera bTree]
    where bTree = buildQuadTree blocks worldSize

toPictures :: CollisionObject a => Color -> Point -> QuadTree a -> Picture
toPictures _ _ EmptyLeaf = Blank
toPictures color cam (Node (pos,(w,h)) _ tl tr bl br) = Pictures [
    Color color (Scale worldScale worldScale (uncurry Translate (pos - cam) (Polygon [(0,0), ( w, 0), (w, h), (0, h)]))),
    toPictures (dim color) cam tl, toPictures (dim color) cam tr, toPictures (dim color) cam bl, toPictures (dim color) cam br]
