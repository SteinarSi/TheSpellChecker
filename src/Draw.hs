
{-# LANGUAGE RankNTypes, TupleSections #-}

module Draw (draw, Drawable(..)) where


import Expr
import Utility



import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Data.Decimal
import Calculus

import Debug.Trace



data Drawable a = Point a a
                | DFunction (Function a)

data World a = World CircleView [Drawable a]
data CircleView = CV {
        center :: (Float, Float),
        radius :: Float
    }


draw :: (Show a, RealFloat a, Enum a) => [Drawable a] -> IO ()
draw ds = play FullScreen white 1 (World defaultView ds) paint move (const id)


move :: Event -> World a -> World a
move e@(EventKey (SpecialKey dir) Down _ _) w@(World (CV (x, y) r) ds) = case dir of
    KeyRight -> World (CV (x + r / (nTicks-1), y) r) ds
    KeyLeft  -> World (CV (x - r / (nTicks-1), y) r) ds
    KeyUp    -> World (CV (x, y + r / (nTicks-1)) r) ds
    KeyDown  -> World (CV (x, y - r / (nTicks-1)) r) ds
    _        -> w
move e@(EventKey (Char c) Down _ _) w@(World (CV (x, y) r) ds) = case c of
    'z' -> World (CV (x, y) (r/2)) ds
    'x' -> World (CV (x, y) (r*2)) ds
    'r' -> World defaultView ds
    _   -> w
move e v = v

nTicks :: Float
nTicks = 21

defaultView :: CircleView
defaultView = CV (0, 0) 10

paint :: (Show a, RealFloat a, Enum a) => World a -> Picture
paint w@(World cv dt) = pictures [grid, xAxe cv, yAxe cv, drawFeatures w]

grid :: Picture
grid = color (greyN 0.65) $ pictures $ concatMap (\x -> [line [(-1000, x), (1000, x)], line [(x, -1000), (x, 1000)]]) $ linspace nTicks (-960, 960)

drawFeatures :: (RealFloat a, Enum a, Show a) => World a -> Picture
drawFeatures (World cv dt) = pictures $ map drawFeature dt
    where drawFeature :: (RealFloat a, Enum a, Show a) => Drawable a -> Picture
          drawFeature (Point x y) = uncurry translate (transcale cv $ tmap realToFrac (x, y)) (circle 2)
          drawFeature (DFunction f) = pictures $ map (line . map (transcale cv . tmap realToFrac)) $ evalAll (tmap realToFrac (bounds cv True)) f

yAxe :: CircleView -> Picture
yAxe cv@(CV (x,y) _) = pictures $ axe : map (\y -> pictures [label y, tick y]) (linspace nTicks (bounds cv False))
    where
          axe     = line [(transcaleX cv x, -1000), (transcaleX cv x, 1000)]
          label y = uncurry translate (-15, transcaleY cv y) $ scale (1/12) (1/12) $ text $ show $ realFracToDecimal 3 y
          tick  y = line [(-3, transcaleY cv y), (3, transcaleY cv y)]

xAxe :: CircleView -> Picture
xAxe cv@(CV (x, y) _) = pictures $ axe : map (\x -> pictures [label x, tick x]) (linspace nTicks (bounds cv True))
    where
          axe     = line [(-1000, transcaleY cv y), (1000, transcaleY cv y)]
          label x = uncurry translate (transcaleX cv x - 12, -13) $ scale (1/12) (1/12) $ text $ show $ realFracToDecimal 3 x
          tick  x = line [(transcaleX cv x, -3), (transcaleX cv x, 3)]

-- Stor takk til Lukas for denne
transcale :: CircleView -> (Float, Float) -> (Float, Float)
transcale (CV (x, y) r) (x', y') = ((x'-x+r)*1920/(2*r)-960, (y'-y+r)*1920/(2*r)-960)

transcaleX :: CircleView -> Float -> Float
transcaleX = (fst .) . (. (,0)) . transcale

transcaleY :: CircleView -> Float -> Float
transcaleY = (snd .) . (. (0,)) . transcale

bounds :: CircleView -> Bool -> (Float, Float)
bounds (CV (x, _) r) True  = (x-r, x+r)
bounds (CV (_, y) r) False = (y-r, y+r)


