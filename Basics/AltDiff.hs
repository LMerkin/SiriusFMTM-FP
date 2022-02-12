-- Alternative Method for Automatic Function Differentiation
--
module AltDiff
where

newtype FnD = FnD (Double, Double) deriving (Show)

mkFnDx     :: Double -> FnD
mkFnDx     x  = FnD (x, 1)

mkFnDconst :: Double -> FnD
mkFnDconst c  = FnD (c, 0)

instance Num FnD where
  (+) (FnD (yl, dl)) (FnD (yr, dr)) = FnD (yl + yr, dl + dr)

  (-) (FnD (yl, dl)) (FnD (yr, dr)) = FnD (yl - yr, dl - dr)

  (*) (FnD (yl, dl)) (FnD (yr, dr)) = FnD (yl * yr, yl * dr + yr * dl)

  abs    (FnD (y,  d))              = FnD (let ay = abs y in (ay, ay * d))

  signum (FnD (y,  _))
    | y /= 0      = FnD (signum y, 0)
    | otherwise   = error "signum in FnD: y=0"

  fromInteger  i = error "fromInteger  not supported on FnD"

instance Fractional FnD where
  (/) (FnD (yl, dl)) (FnD (yr, dr)) = FnD (yl / yr, dl / yr - dr * yl/(yr*yr))

  fromRational r = error "fromRational not supported in FnD"

instance Floating   FnD where
  (**) (FnD (yl, dl)) (FnD (yr, dr)) =
    let p  = yl**yr
    in  FnD (p, p * (dl * yr / yl + dr * log yl))

  pi = FnD (pi :: Double, 0)

  exp (FnD (y, d)) = let ey = exp y in FnD (ey, ey * d)

  log (FnD (y, d)) = FnD (log y,   d / y)

  sin (FnD (y, d)) = FnD (sin y,   (cos y) * d)

  cos (FnD (y, d)) = FnD (cos y, - (sin y) * d)

-- Also: asin,  acos,  atan, sinh, cosh,
--       asinh, acosh, atanh
--

-- Example: f2(x) = 5 * cos(x^2):
--
test :: Double -> FnD
test x = c * cos (x' * x')
  where
  -- Lifing x::Double to FnD:
  x' = mkFnDx x
  -- Lifting a const  to FnD:
  c  = mkFnDconst 5

