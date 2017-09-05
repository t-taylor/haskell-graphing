module Bindings
  ( displayLine
  ) where
import Graphics.UI.GLUT

-- Create a DisplayCallback from a function and resolution
displayLine :: (Double -> Double) -> Double -> DisplayCallback
displayLine f res = do
  clear [ColorBuffer]
  renderPrimitive Points $
    mapM_ (\(x, y) -> vertex $ Vertex3 x y 0) $ gscale $ graph res f 0
  flush

-- Scale graph
gscale :: [(Double, Double)] -> [(Double, Double)]
gscale ys = reduce (maxval ys) ys
  where reduce val ((x, y):ys) = (x, 2 * (y / val) - 1) : reduce val ys
        reduce _ [] = []
        maxval [(x,y)] = y
        maxval ((x, y):ys) = max y $ maxval ys
        maxval [] = 0

-- Generate points
graph :: Double -> (Double -> Double) -> Double -> [(Double, Double)]
graph res f x = if x < 2 
                then (x - 1, f x) : graph res f (res + x)
                else [(x - 1, f x)]
