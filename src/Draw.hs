
{-# LANGUAGE RankNTypes #-}

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

type Zoom = Float
type HorizontalOffset = Float
type VerticalOffset = Float

data View a = View Zoom HorizontalOffset VerticalOffset [Drawable a]
data World a = World CircleView [Drawable a]


draw :: (Show a, RealFloat a, Enum a) => [Drawable a] -> IO ()
draw ds = play FullScreen white 1 (World (CV 0 10) ds) paint move (const id)


move :: Event -> World a -> World a
move e@(EventKey (SpecialKey dir) Down _ _) w@(World cv ds) = case dir of
    KeyRight -> World (walk cv True ) ds
    KeyLeft  -> World (walk cv False) ds
    KeyUp    -> w
    KeyDown  -> w -- TODO
    _        -> w
move e@(EventKey (Char c) Down _ _) w@(World cv ds) = case c of
    'z' -> World (zoom cv True ) ds
    'x' -> World (zoom cv False) ds
    _   -> w
move e v = v




data CircleView = CV {
        center :: Float,
        radius :: Float
    }

zoom :: CircleView -> Bool -> CircleView
zoom cv True  = CV (center cv) (radius cv / 2)
zoom cv False = CV (center cv) (radius cv * 2)


walk :: CircleView -> Bool -> CircleView
walk cv True  = CV (center cv + radius cv / nTicks) (radius cv)
walk cv False = CV (center cv - radius cv / nTicks) (radius cv)


nTicks :: Float
nTicks = 25


paint :: (Show a, RealFloat a, Enum a) => World a -> Picture
paint w@(World cv dt) = pictures [xAxe cv, drawFeatures w]

drawFeatures :: (RealFloat a, Enum a, Show a) => World a -> Picture
drawFeatures (World cv dt) = pictures $ map drawFeature dt
    where drawFeature :: (RealFloat a, Enum a, Show a) => Drawable a -> Picture
          drawFeature (Point x y) = undefined  --translate (transcale cv x) (transcale y) (circle 2)
          drawFeature (DFunction f) = pictures $ map (line . map (tmap (transcale cv . realToFrac))) $ evalAll (tmap realToFrac (bounds cv)) f

xAxe :: CircleView -> Picture
xAxe cv = pictures $ map (\x -> pictures [label x, tick x]) $ linspace nTicks (bounds cv)
    where f = transcale cv
          label x = translate (f x - 10) (-10) $ scale (1/12) (1/12) $ text $ show $ realFracToDecimal 3 x
          tick  x = line [(f x, 3), (f x, -3)]

transcale :: CircleView -> Float -> Float
transcale (CV c r) e = (e-c+r)*1920/(2*r) - 960

bounds :: CircleView -> (Float, Float)
bounds cv = (center cv - radius cv, center cv + radius cv)


