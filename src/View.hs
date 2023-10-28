{-# LANGUAGE NamedFieldPuns #-}
module View where
import Graphics.Gloss
import Model
import GHC.Data.Bitmap
import Data.Map hiding (map)
import GHC.Driver.Session (positionIndependent)


worldScale :: Float
worldScale = 4 * 8

missingTexture :: Picture
missingTexture = Pictures [ 
    Color magenta   (Polygon [(0,0), (0,0.5), (0.5,0.5), (0.5,0)]),
    Color black     (Polygon [(0.5,0), (0.5,0.5), (1,0.5), (1,0)]),
    Color magenta   (Polygon [(0.5,0.5), (0.5,1), (1,1), (1,0.5)]),
    Color black     (Polygon [(0,0.5), (0,1), (0.5,1), (0.5,0.5)])]

view :: Map String (Map String Animation) -> World -> IO Picture
view map w = do 
    let pics = viewPure map w
    return (Pictures pics)

viewPure :: Map String (Map String Animation) -> World -> [Picture]
viewPure map w@(World {
    player, enemies, blocks, points,
    timeLeft, camera, gameState, worldSize}) = getFrame player : (Prelude.map getFrame enemies ++ Prelude.map getFrame blocks)
    where getFrame :: CollisionObject a => a -> Picture
          getFrame obj = getFrame' obj (getCurrentAnimation obj)
          getFrame' obj (Just a) = Scale worldScale worldScale (uncurry Translate (getPosition obj - camera) (frames a !! index a))
          getFrame' obj Nothing  = Scale worldScale worldScale (uncurry Translate (getPosition obj - camera) missingTexture)
          
