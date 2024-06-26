{-# LANGUAGE NamedFieldPuns #-}
module View where
import Graphics.Gloss
import Model
import Data.Map hiding (filter, map)
import qualified Data.Map as Map hiding (filter)
import Data.Maybe (isJust)
import Prelude hiding (flip)
import QuadTree

-- oooooh scary IO
view ::  World -> IO Picture
view w = do
    let pics = reverse ( viewPure w)
    return (Pictures pics )

-- safe pure stuff
viewPure :: World -> [Picture]
viewPure w = viewUI w ++ viewWorld w

viewUI :: World -> [Picture]
viewUI w@(World {
    player, enemies, blocks, pickupObjects, points,
    timeLeft, camera, gameState, worldSize}) 
    | isAlive player && gameState == GoMode = [Color white (Translate (-500) 380 (Scale 0.2 0.2 (Text ("Time until Heart Failure: " ++ show timeLeft)))),
                        Color white (Translate 30 380 (Scale 0.2 0.2 (Text ("Money collected for surgery: " ++ show points))))]
    | gameState == Win = [Color white (Translate (-420) 0 (Scale 0.4 0.4 (Text "You got to the Hospital in time!"))),
                          Color white (Translate (-215) (-35) (Scale 0.3 0.3 (Text "press escape to restart")))]
    | gameState == Pause = [Color white (Translate (-355) 5 (Scale 0.5 0.5 (Text "Paused")))]
    | gameState == Loading = [Color white (Translate (-355) 5 (Scale 0.5 0.5 (Text "Loading")))]
    | otherwise      = [Color white (Translate (-355) 5 (Scale 0.5 0.5 (Text "Died from Heart Attack"))),
                        Color white (Translate (-215) (-30) (Scale 0.3 0.3 (Text "press escape to restart")))]

viewWorld :: World -> [Picture]
viewWorld w@(World { gameState = Loading}) = [Blank]
viewWorld w@(World {
    player, enemies, blocks, pickupObjects, points,
    timeLeft, camera, gameState, worldSize, backGround}) =
        getFrame camera player  :
        map (getFrame camera) enemies ++
        map (getFrame camera) pickupObjects ++
        map (getFrame camera) blocks ++
        map (getFrame camera) pipes ++
        [(Scale worldScale worldScale . uncurry Translate (-camera * (0.5, 0))) backGround]
        -- ++[viewQT w] -- uncomment to view the quad tree
    where nblocks = filter (\b -> getName b /= "Pipe") blocks
          pipes   = filter (\b -> getName b == "Pipe") blocks

getFrame :: CollisionObject a => Camera -> a -> Picture
getFrame c obj  | isAlive obj = --getFrame' obj c (getCurrentAnimation obj) 
                                if facingLeft obj then worldToScreen (flip (getFrame' obj c (getCurrentAnimation obj)))
                                else worldToScreen (getFrame' obj c (getCurrentAnimation obj))
                | otherwise   = Blank
    where worldToScreen | getName obj /= "Pipe" = Scale worldScale worldScale . uncurry Translate (getPos obj - c)
                        | otherwise             = Scale worldScale worldScale . uncurry Translate (getPos obj - c - (0, 8 - snd (snd (getBB obj))))

flip :: Picture -> Picture
flip = Translate 1 0 . Scale (-1) 1

getFrame' :: CollisionObject a => a -> Camera -> Maybe Animation -> Picture
getFrame' obj c (Just a) =  frames a !! index a
getFrame' obj c Nothing  =  Scale x y missingTexture
    where (_, (x,y))     = getBB obj


-- no longer used, took too much effort to throw away
{- 

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
 -}
