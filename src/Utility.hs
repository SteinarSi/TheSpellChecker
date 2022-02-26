

module Utility where


import Data.Number.RealCyclotomic (RealCyclotomic)
import Data.Text (Text, unpack)
import qualified Prelude as P
import Prelude hiding (pi)


realToRat :: (Real a, Fractional b) => a -> b
realToRat = fromRational . realToFrac


e :: RealCyclotomic
e = 2.71828182845904523536028747

pi :: RealCyclotomic
pi = realToFrac P.pi

failT :: MonadFail m => Text -> m a
failT = fail . unpack