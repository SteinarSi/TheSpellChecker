

module Utility where


import Data.Number.RealCyclotomic (RealCyclotomic, toReal, sqrtRat)
import Data.Text (Text, unpack)
import TextShow --(TextShow, showb)
import Data.Number.CReal


realToRat :: (Real a, Fractional b) => a -> b
realToRat = fromRational . realToFrac


e :: (RealFloat n, TextShow n) => n
e = 2.71828182845904523536028747


instance TextShow CReal where
    showb = fromString . showCReal 3

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


-- Dette er ren juks. Jeg kan ta alle Floating-operasjoner på cyclotomiske tall bare ved
--  å eksportere tallene ineksakt frem og tilbake. Dette gjør at mange av operasjonene
--  ikke blir helt nøyaktige, men det er fortsatt bedre enn å bare bruke Floats.
cheat :: (RealFloat b, Real a, Fractional c) => (b -> a) -> RealCyclotomic -> c
cheat f = realToRat . f . toReal



