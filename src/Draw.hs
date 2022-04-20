
module Draw (draw, Drawable(..)) where


import Expr

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


import Debug.Trace



data Drawable a = Point a a
                | Function (Function a)

type Zoom = Float
type HorizontalOffset = Float
type VerticalOffset = Float

data View a = View Zoom HorizontalOffset VerticalOffset [Drawable a]

draw :: (Show a, RealFloat a) => [Drawable a] -> IO ()
draw ds = play FullScreen white 1 (View 1 0 0 ds) drawLines move (const id)


move :: Event -> View a -> View a
move e@(EventKey (SpecialKey dir) Down _ _) w@(View z h v ds) = case dir of
    KeyRight -> View z h (v+1) ds
    KeyLeft  -> View z h (v-1) ds
    KeyUp    -> View z (h+1) v ds
    KeyDown  -> View z (h-1) v ds
    _        -> w
move e@(EventKey (Char c) Down _ _) w@(View z h v ds) = case c of
    'z' -> View (z+0.5) h v ds
    'x' -> View (z-0.5) h v ds
    _   -> w
move e v = v



drawLines :: View a -> Picture
drawLines v = pictures [grid v, axes v]

grid :: View a -> Picture 
grid (View z h v _) = color (greyN 0.5) $ pictures []

axes :: View a -> Picture
axes (View z h v _) = color black $ pictures [line [(-1000, -stepsize z * h), (1000, -stepsize z * h)], line [(-stepsize z * v, -540), (-stepsize z * v, 540)]]

stepsize :: Float -> Float
stepsize z = 10**z