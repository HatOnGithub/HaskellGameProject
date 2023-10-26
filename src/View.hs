module View where
import Graphics.Gloss (Picture)
import Model
import GHC.Data.Bitmap




view :: World -> IO Picture
view = return . viewPure

viewPure :: World -> Picture
viewPure world = undefined