

module Utility where


import Data.Number.RealCyclotomic (RealCyclotomic, toReal, sqrtRat)
import Data.Text (Text, unpack, pack)
import TextShow --(TextShow, showb)
import Data.Number.CReal


realToRat :: (Real a, Fractional b) => a -> b
realToRat = fromRational . realToFrac

applyNtimesM :: Monad m => Int -> (a -> m a) -> a -> m a 
applyNtimesM n f a = iterate (>>= f) (pure a) !! n

deleteNth :: Integer -> [a] -> [a]
deleteNth _ [] = []
deleteNth 0 (_:xs) = xs
deleteNth n (x:xs) = x : deleteNth (n-1) xs

for :: [a] -> (a -> b) -> [b]
for = flip map

instance TextShow CReal where
    showb = fromString . showCReal 3

instance TextShow RealCyclotomic where
    showb = fromText . pack . show

instance Floating RealCyclotomic where
    pi = 3.141592653589793238462643383
    (**) a b = realToRat $ toReal a ** toReal b
    sqrt  = sqrtRat . realToRat . toReal
    acos  = cheat acos
    asin  = cheat asin
    atan  = cheat atan
    cos   = cheat cos
    sin   = cheat sin
    tan   = cheat tan
    acosh = cheat acosh
    asinh = cheat asinh
    atanh = cheat atanh
    cosh  = cheat cosh
    sinh  = cheat sinh
    tanh  = cheat tanh
    exp   = cheat exp
    log   = cheat log

-- En klasse av ting som har en alternativ show-funksjon, for debugging.
class Debug a where
    debug :: a -> String


-- Dette er ren juks. Jeg kan ta alle Floating-operasjoner på cyclotomiske tall bare ved
--  å eksportere tallene ineksakt frem og tilbake. Dette gjør at mange av operasjonene
--  ikke blir helt nøyaktige, men det er fortsatt bedre enn å bare bruke Floats.
cheat :: (RealFloat b, Real a, Fractional c) => (b -> a) -> RealCyclotomic -> c
cheat f = realToRat . f . toReal




