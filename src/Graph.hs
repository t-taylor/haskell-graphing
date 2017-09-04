import Graphics.UI.GLUT
import Bindings
import Collatz

main :: IO ()
main = do
  (_progName, _args) <- getArgsAndInitialize
  _window <- createWindow "Graph Display"
  displayCallback $= display (gcollatz 0.0001) 0.0001
  mainLoop

