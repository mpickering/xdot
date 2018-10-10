{- |
   Module      : Graphics.XDot.Viewer
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

   This module draws the operations of an xdot graph using Cairo and Pango on a
   Gtk canvas.
 -}
module Graphics.XDot.Viewer (
  drawAll
)
where

import Debug.Trace

import Graphics.XDot.Types hiding (w, h, filled, alignment, text, name, size)

{-
import Graphics.UI.Gtk (PangoRectangle(..), layoutSetFontDescription,
  layoutGetExtents, layoutContextChanged, fontDescriptionFromString,
  fontDescriptionSetSize, showLayout, cairoContextSetFontOptions,
  cairoContextGetFontOptions, layoutGetContext, createLayout)
import Graphics.Rendering.Cairo hiding (x, y)
-}

import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Canvas hiding (lineWidth)
import Graphics.UI.Threepenny.Canvas.Utils


import Control.Monad.State hiding (State)
import qualified Control.Monad.State as MS

type RGBA = (Double, Double, Double, Double)

data DState = DState
  { fontName    :: String
  , fontSize    :: Double
  , lineWidth   :: Double
  , lineStyle   :: [Double]
  , filledColor :: RGBA
  , strokeColor :: RGBA
  }
type DrawState a = MS.StateT DState UI a

-- | Draw an xdot graph, possibly highlighting a node.
drawAll :: Eq t =>
     Canvas
  -> Object t -- ^ id of the node to highlight
  -> Rectangle -- ^ dimensions of the graph, as returned by 'Graphics.XDot.Parser.getSize'
  -> [(Object t, Operation)] -- ^ operations, as returned by 'Graphics.XDot.Parser.getOperations'
  -> UI [(Object t, Rectangle)] -- ^ dimensions of the rendered nodes on the screen
drawAll c hover (_,_,sw,sh) ops = do
  let scalex = 1
      scaley = -1
      offsetx = -0.5 * sw
      offsety = 0.5 * sh
  save c
  translate offsetx offsety c
  scale scalex scaley c

  boundingBoxes <- evalStateT (mapM (draw c hover) ops) $ DState "" 1 1 [] (1,1,1,1) (0,0,0,1)

  restore c
  return
    $ map (\(o, (x,y,w,h)) -> (o, (x*scalex+offsetx,y*scaley+offsety,w,h)))
    $ concat boundingBoxes

stylizedDraw :: Eq t => Canvas -> Bool -> Object t -> Object t -> UI a -> DrawState ()
stylizedDraw c filled mn hover renderOps = do
  (rf,gf,bf,af) <- gets filledColor
  (rs,gs,bs,as) <- gets strokeColor
  lWidth <- gets lineWidth
--  lStyle <- gets lineStyle

  lift $ do
    when filled $ do
      if mn /= None && mn == hover
        then setSourceRGBA 1 0.8 0.8 1 c
        else setSourceRGBA rf gf bf af c

      save c
      renderOps
      restore c

      fill c

    c # set' UI.lineWidth lWidth
--setDash lStyle 0

    if mn /= None && mn == hover
      then setSourceRGBA 1 0 0 1 c
      else setSourceRGBA rs gs bs as c

    save c
    renderOps
    restore c

    stroke c

draw :: Eq t => Canvas -> Object t -> (Object t, Operation) -> DrawState [(Object t, Rectangle)]
draw _ _ (_, o) | traceShow o False = undefined
draw c hover (mn, Ellipse (x,y) w h filled) = do
  stylizedDraw c filled hover mn $ do
    translate x y c
    scale w h c
    moveTo (1, 0) c
    arc (0, 0) 1 0 (2 * pi) c

  return $ case mn of
    None -> []
    o -> [(o, (x - w, y + h, 2 * w, 2 * h))]

draw c hover (mn, Polygon a@((x,y):xys) filled) = do
  stylizedDraw c filled hover mn $ do
    beginPath c
    moveTo (x, y) c
    mapM_ (\p -> lineTo p c) xys
    closePath c

  let xs = x : map fst a
  let ys = y : map snd a

  return $ case mn of
    None -> []
    o -> [(o, (minimum xs, maximum ys, maximum xs - minimum xs, maximum ys - minimum ys))]

draw _ _ (_, Polygon [] _) = return []

draw c hover (mn, Polyline a@((x,y):xys)) = do
  stylizedDraw c False hover mn $ do
    moveTo (x, y) c
    mapM_ (\p -> lineTo p c) xys

  let xs = x : map fst a
  let ys = y : map snd a

  return $ case mn of
    None -> []
    o -> [(o, (minimum xs, maximum ys, maximum xs - minimum xs, maximum ys - minimum ys))]

draw _ _ (_, Polyline []) = return []

draw c hover (mn, BSpline ((x,y):xys) filled) = do
  stylizedDraw c filled hover mn $ do
    moveTo (x, y) c
    drawBezier xys

  return $ case mn of
    None -> []
    o -> [ (o, (x  - 15, y  + 15, 30, 30))
         , (o, (xe - 15, ye + 15, 30, 30))
         ]

  where drawBezier ((x1,y1):(x2,y2):(x3,y3):xys2) = do
          curveTo x1 y1 x2 y2 x3 y3 c
          drawBezier xys2
        drawBezier _ = return ()
        (xe,ye) = last xys

draw _ _ (_, BSpline [] _) = return []

draw c hover (mn, Text (x,y) alignment w display_text) = do
  -- 6 is the width of a monospaced font
  let (w2,h2) = (fromIntegral (6 * length display_text),10)

  let (f, w3, h3, descent) = if w2 > w
        then (w / w2, w,  h2 * w / w2, 4 * w / w2)
        else (1,      w2, h2,          4)

  let x3 = case alignment of
             LeftAlign   -> x
             CenterAlign -> x - 0.5 * w3
             RightAlign  -> x -       w3
      y3 = y + h3 - descent

  stylizedDraw c False hover mn $ do
    moveTo (x3,y3) c
    scale f (-f) c
    c # setSourceRGB 0 0 0
    fillText display_text (x3, -y3) c

  return $ case mn of
    None -> []
    o -> [(o, (x3, y3, w3, h3))]

draw _ _ (_, Color (r,g, b, a) filled) = do
  let color = (r * 255, g * 255, b * 255, a * 255)
  modify (\s -> if filled
    then s{filledColor = color}
    else s{strokeColor = color})
  return []

draw _ _ (_, Font size name) = do
  modify (\s -> s{fontName = fixedName, fontSize = size})
  return []

  -- Pango does not like "Times-Roman", but works with "Times Roman".
  -- Graphviz handles this in plugin/pango/gvtextlayout_pango.c
  where fixedName = map fixName name
        fixName '-' = ' '
        fixName x   = x

draw _ _ (_, Style x) = do
  case x of -- TODO: Some styles missing
    "solid"  -> modify (\s -> s{lineStyle = []}) -- always on
    "dashed" -> modify (\s -> s{lineStyle = [6,6]}) -- 6 pts on, 6 pts off
    "dotted" -> modify (\s -> s{lineStyle = [2,4]}) -- 2 pts on, 4 pts off
    _ -> return ()
  return []

draw _ _ (_, Image{}) = return [] -- TODO

draw _ _ (_, FontCharacteristics{}) = return [] -- TODO

setSourceRGB :: Double -> Double -> Double -> Canvas -> UI ()
setSourceRGB r g b c =
  c # set' fillStyle (solidColor (RGB (round r) (round g) (round b)))

setSourceRGBA :: Double -> Double -> Double -> Double -> Canvas -> UI ()
setSourceRGBA r g b a c =
  c # set' fillStyle (solidColor (RGBA (round r) (round g) (round b) a))
