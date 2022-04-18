
module Draw (draw, Drawable(..)) where


import Expr

import Graphics.Gloss



data Drawable a = Point a a
                | Function (Function a)

draw :: (Show a, RealFloat a) => [Drawable a] -> IO ()
draw x = play FullScreen white 20 x (const (circle 80)) (flip const) (flip const)