module OperatorOverloading (
  Point,
  (+),
  (-),
  (*),
  negate,
) where

import           Prelude hiding (negate, (*), (+), (-))
import qualified Prelude as P

type Point a = (a, a)

(+) :: P.Num a => Point a -> Point a -> Point a
(x1, y1) + (x2, y2) = (x1 P.+ x2, y1 P.+ y2)

(-) :: P.Num a => Point a -> Point a -> Point a
(x1, y1) - (x2, y2) = (x1 P.- x2, y1 P.- y2)

(*) :: P.Num a => a -> Point a -> Point a
s * (x, y) = (s P.* x, s P.* y)

negate :: P.Num a => Point a -> Point a
negate (x, y) = (-x, -y)
