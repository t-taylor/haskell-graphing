import Graphics.UI.GLUT
import Bindings
import Linear

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  let _display = case _args !! 0 of
                "coll" -> (intftodouble collatz) <-- (_res _args)
                "sin" -> displayLine sin (_res _args)
                _ -> (\x y -> 0) <-- 0.0
  _window <- createWindow _progName
  displayCallback $= _display
  mainLoop

-- Passes the resolution to the funciton as well as the display for scaling
(<--) :: (Double -> Double -> Double) -> Double -> DisplayCallback
(<--) f res = displayLine (f res) res

_res :: [String] -> Double
_res _args = read (_args !! 1) :: Double
