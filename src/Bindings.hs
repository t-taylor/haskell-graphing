module Bindings
  ( display
  ) where
import Graphics.UI.GLUT

display :: (Double -> Double) -> Double -> DisplayCallback
display f res = do
  clear [ColorBuffer]
  renderPrimitive Points $
    mapM_ (\(x, y) -> vertex $ Vertex3 x y 0) $ gscale $ graph res f 0
  flush

gscale :: [(Double, Double)] -> [(Double, Double)]
gscale ys = reduce (maxval ys) ys
  where reduce val ((x, y):ys) = (x, 2 * (y / val) - 1) : reduce val ys
        reduce _ [] = []
        maxval [(x,y)] = y
        maxval ((x, y):ys) = max y $ maxval ys
        maxval [] = 0

graph :: Double -> (Double -> Double) -> Double -> [(Double, Double)]
graph res f x = if x < 2 
                then (x - 1, f (x + 1)) : graph res f (res + x)
                else [(x - 1, f (x + 1))]
