-- Example of an XDot drawing with clickable nodes

import System.Environment
import Control.Monad
import Control.Monad.Trans.Class
import Data.IORef

import qualified Data.Text.Lazy.IO as L

import Data.GraphViz
import qualified Data.GraphViz.Types.Generalised as G
import qualified Data.GraphViz.Types.Graph as R
import Data.GraphViz.Commands.IO

import Graphics.XDot.Parser
import Graphics.XDot.Viewer
import Graphics.XDot.Types hiding (w, h)

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

data State = State
  { objects  :: ([(Object String, Operation)], Rectangle)
  , bounds   :: [(Object String, Rectangle)]
  , mousePos :: Point
  , hover    :: Object String
  }

main :: IO ()
main = do
  args <- getArgs
  quitWithoutGraphviz "Cannot continue: Graphviz is not installed"
  if length args == 1 then run $ head args else error "Usage: Demo file.dot"

run :: String -> IO ()
run file = do
  dotText <- L.readFile file
  -- If dg is a G.DotGraph it fails when there's a subgraph in it
  let dg = parseDotGraph dotText :: R.DotGraph String

  -- You can choose another graphviz command by changing Dot to Neato, TwoPi, Circo or Fdp
  xdg <- graphvizWithHandle Dot dg (XDot Nothing) hGetDot

  let objs = (getOperations xdg, getSize xdg)

  --putStrLn $ show xdg
  --putStrLn $ show objs

  state <- newIORef $ State objs [] (0,0) None

  startGUI (defaultConfig  { jsPort = Just 8080 }) (setup state)

canvasSize = 1000

setup :: IORef State -> Window -> UI ()
setup state window = do
  return window # set UI.title "Canvas - Examples"

  canvas <- UI.canvas
              # set UI.height canvasSize
              # set UI.width  canvasSize
              # set UI.style [("border", "solid black 1px"), ("background", "#eee")]
              # set UI.textFont "10px monospace"

  body <- getBody window #+
    [ column [element canvas] ]

  --setupGUI window body canvas dummy
  liftIO $ getChar
  redraw canvas state
  return ()

{-
  on canvas draw $ do
    redraw canvas state

  on canvas motionNotifyEvent $ do
    (x,y) <- eventCoordinates
    lift $ do
      modifyIORef state (\s -> s {mousePos = (x,y)})
      tick canvas state
      return True

  on canvas buttonPressEvent $ do
    button <- eventButton
    eClick <- eventClick
    lift $ do
        when (button == LeftButton && eClick == SingleClick) $
          click state dg
        return True

  widgetShowAll window
  on window destroyEvent $ lift $ mainQuit >> return True
  mainGUI

click :: IORef State -> R.DotGraph String -> IO ()
click state _dg = do
  s <- readIORef state

  case hover s of
    Node t -> putStrLn $ "Node clicked: " ++ t
    Edge f t -> putStrLn $ "Edge clicked: " ++ f ++ " -> " ++ t
    _ -> return ()

tick :: UI.Canvas -> IORef State -> IO ()
tick canvas state = do
  oldS <- readIORef state
  let oldHover = hover oldS

  modifyIORef state $ \s' -> (
    let (mx, my) = mousePos s'
        check (name', (x,y,w,h)) =
          if x <= mx && mx <= x + w &&
             y <= my && my <= y + h
          then name' else None

        validOne (None:xs) = validOne xs
        validOne (x:_) = x
        validOne _ = None
    in s' {hover = validOne $ map check (bounds s')}
    )

  s <- readIORef state
  unless (oldHover == hover s) $ widgetQueueDraw canvas
-}
redraw :: UI.Canvas -> IORef State -> UI ()
redraw canvas state = do
  s <- liftIO $ readIORef state
  let (rw, rh) = (1000,1000)

  let (ops, size'@(_,_,sw,sh)) = objects s

  -- Proportional scaling
  let scalex = min (fromIntegral rw / sw) (fromIntegral rh / sh)
      scaley = scalex
      offsetx = 0.5 * fromIntegral rw
      offsety = 0.5 * fromIntegral rh
  save canvas
  translate offsetx offsety canvas
  scale scalex scaley canvas

  result <- drawAll canvas (hover s) size' ops

  restore canvas

  let boundingBoxes = map (\(o, (x,y,w,h)) -> (o, (x*scalex+offsetx,y*scaley+offsety,w*scalex,h*scaley))) result

  liftIO $ modifyIORef state (\s' -> s' {bounds = boundingBoxes})

save :: UI.Canvas -> UI ()
save c = runFunction $ ffi "%1.getContext('2d').save();" c

restore :: UI.Canvas -> UI ()
restore c = runFunction $ ffi "%1.getContext('2d').restore();" c

translate :: Double -> Double -> UI.Canvas -> UI ()
translate x y c = runFunction $ ffi "%1.getContext('2d').translate(%2, %3);" c x y

scale :: Double -> Double -> UI.Canvas -> UI ()
scale x y c = do
  runFunction $ ffi "%1.getContext('2d').scale(%2, %3);" c x y
