module Bindings
  ( displayLine
  ) where
import Graphics.UI.GLUT

-- Better DisplayCallback
displayRange :: Double -> Double -> (Double -> Double) -> PrimitiveMode -> DisplayCallback
displayRange min max f brush = do
  clear [ColorBuffer]
  renderPrimitive brush $
    mapM_ (\(x, y) -> vertex $ Vertex3 x y 0) $ scaledCoords min max f
  flush

scaledCoords :: Double -> Double -> (Double -> Double) -> [(Double, Double)]
scaledCoords min max f = xyscale $ genPoint min max ((max - min) / 1000) f

genPoint n max res f = if n >= max
                          then [(n, f n)]
                          else (n, f n) : genPoint (n + res) max res f

-- I should really use matricies instead (This is also incorrect at the
-- moment)
xyscale :: (Fractional t, Fractional t1, Ord t, Ord t1) => [(t1, t)] -> [(t1, t)]
xyscale xs = reduce (maxvals xs) (minvals xs) xs
  where reduce (maxx, maxy) (minx, miny) ((x, y) : ys) 
          = (moddedx , moddedy) : reduce (mx, my) ys
          where moddedx = (x - minx) * (2 / (maxx - minx))
                moddedy = (y - miny) * (2 / (maxy - miny))
        reduce _ [] = []

maxvals :: (Ord t, Ord t1, Num t1, Num t) => [(t1, t)] -> (t1, t)
maxvals [(x,y)] = (x, y)
maxvals (xt:ys) = maxt xt $ maxvals ys
  where maxt (x,y) (j,k) = (max x j, max y k)
maxvals [] = (0, 0)

minvals :: (Ord t, Ord t1, Num t1, Num t) => [(t1, t)] -> (t1, t)
minvals [(x,y)] = (x, y)
minvals (xt:ys) = mint xt $ minvals ys
  where mint (x,y) (j,k) = (min x j, min y k)
minvals [] = (0, 0)


-- Create a DisplayCallback from a function and resolution
displayLine :: (Double -> Double) -> Double -> DisplayCallback
displayLine f res = do
  clear [ColorBuffer]
  renderPrimitive Points $
    mapM_ (\(x, y) -> vertex $ Vertex3 x y 0) $ yscale $ graph res f 0
  flush

-- Scale graph
yscale :: [(Double, Double)] -> [(Double, Double)]
yscale ys = reduce (maxval ys) ys
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
