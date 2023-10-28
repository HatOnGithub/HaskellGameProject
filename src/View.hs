{-# LANGUAGE NamedFieldPuns #-}
module View where
import Graphics.Gloss
import Model
import GHC.Data.Bitmap
import Data.Map hiding (map)
import GHC.Driver.Session (positionIndependent)


worldScale :: Float
worldScale = 4 * 8


view :: Map String (Map String Animation) -> World -> IO Picture
view map w = do 
    let pics = viewPure map w
    return (Pictures pics)

viewPure :: Map String (Map String Animation) -> World -> [Picture]
viewPure map w@(World {
    player, enemies, blocks, pickupObjects, points,
    timeLeft, camera, gameState, worldSize}) = getFrame player : (Prelude.map getFrame enemies ++ Prelude.map getFrame blocks ++ Prelude.map getFrame pickupObjects)
    where getFrame  obj = getFrame' obj (getCurrentAnimation obj)
          getFrame' obj (Just a) = Scale worldScale worldScale (uncurry Translate (getPosition obj - camera) (frames a !! index a))
          getFrame' obj Nothing  = Scale worldScale worldScale (uncurry Translate (getPosition obj - camera) missingTexture)
          
