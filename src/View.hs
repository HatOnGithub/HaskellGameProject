{-# LANGUAGE NamedFieldPuns #-}
module View where
import Graphics.Gloss
import Model
import GHC.Data.Bitmap
import Data.Map hiding (map)
import GHC.Driver.Session (positionIndependent)
import QuadTree


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
    timeLeft, camera, gameState, worldSize}) = 
        if isAlive player then [Blank] else [Translate (-120) 5 (Scale 0.5 0.5 (Text "Skill Issue"))]

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
getFrame c obj = if isAlive obj then getFrame' obj c (getCurrentAnimation obj) else Blank

getFrame' :: CollisionObject a => a -> Camera -> Maybe Animation -> Picture
-- found animation!
getFrame' obj c (Just a) = Scale worldScale worldScale (uncurry Translate (getPosition obj - c) (frames a !! index a))
-- uh, where is it?
getFrame' obj c Nothing  = Scale worldScale worldScale (uncurry Translate (getPosition obj - c) missingTexture)

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
    Color color (Scale worldScale worldScale (uncurry Translate (pos - cam) (Line [(0,0), ( w, 0), (w, h), (0, h)]))),
    quadTreeToPictures (dim color) cam tl, 
    quadTreeToPictures (dim color) cam tr, 
    quadTreeToPictures (dim color) cam bl, 
    quadTreeToPictures (dim color) cam br]

