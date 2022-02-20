-- vim:ts=2:et
-- ========================================================================= --
--                              "TimeSteppers.hs":                           --
--          Time Steppers for Solving Systems of ODEs: x' = f(x,t)           --
-- ========================================================================= --
module TimeSteppers
(
  TimeStepperMethod(..), integrateODEsL
)
where

import qualified Common

-------------------------------------------------------------------------------
-- "TimeStepperL":                                                           --
-------------------------------------------------------------------------------
-- This is a common type of List-based TimeSteppers: Type of a function perfor-
-- ming one ODE integratio step from "t" to "t'", for a given initial condition
-- "x(t)" and the required abs error "eps".
-- NOTE THAT, due to automatic accuracy / step control, the  final  integration
-- point "t''" may be different from the originally-required "t'";  integration
-- step may be reduced, but never increased.
-- Returns the solution at the final point: (x(t''), t''):
--
type TimeStepperL =
    ([Double] -> Double -> [Double]) -> [Double] -> Double -> Double ->
     -- f                               x(t)        t         t'
     Double -> ([Double],   Double)
     -- eps    (x(t''),     t'')

-------------------------------------------------------------------------------
-- "rkf5L": Runge-Kutta-Fehlberg Method of order 4-5, List-based:            --
-------------------------------------------------------------------------------
rkf5L :: TimeStepperL

rkf5L  f x t t' eps = res
  where
  -- Times:
  tau, t1, t2, t3,     t5 :: Double
  -- RHSs :
  f0,  f1, f2, f3, f4, f5 :: [Double]

  tau = Common.assert (t' /= t) (t' - t)
  f0  = f x t

  b1  = lc2 (0.25 * tau) f0 1.0 x
  t1  = t +  0.25 * tau
  f1  = f b1 t1

  b2  = lc3 (0.09375 * tau) f0 (0.28125 * tau) f1 1.0 x
  t2  = t +  0.375   * tau
  f2  = f b2 t2

  b3  = lc4 (1932.0  / 2197.0 * tau) f0 (-7200.0 / 2197.0 * tau) f1
            (7296.0  / 2197.0 * tau) f2 1.0 x
  t3  = t + (  12.0  / 13.0)  * tau
  f3  = f b3 t3

  b4  = lc5 ( 439.0  / 216.0  * tau) f0 (-8.0            * tau) f1
            (3680.0  / 513.0  * tau) f2 (-845.0 / 4104.0 * tau) f3 1.0 x
  -- NB: "f4" is evaluated at t4 = t' = t + tau:
  f4  = f b4 t'

  b5  = lc6 (-8.0    / 27.0   * tau) f0 ( 2.0            * tau) f1
            (-3544.0 / 2565.0 * tau) f2 (1859.0 / 4104.0 * tau) f3
            (-0.275           * tau) f4 1.0 x
  t5  = t + 0.5 * tau
  f5  = f b5 t5

  -- Error estimate  (NB: "f1" is not used here):
  e :: [Double]
  e  = lc5 (1.0 / 360.0)  f0 (-128.0 / 4275.0) f2 (-2197.0 / 75240.0) f3
           0.02           f4 ( 2.0   / 55.0)   f5
  err = (abs tau) * (c0norm e)

  -- x' :: Result    (NB: "f1" is not used here):
  res =
    -- NB: if eps <= 0, do not use error / step control at all:
    if eps <= 0 || err < eps
    then
      let
        x' = lc6 (   16.0 /   135.0 * tau) f0 (6656.0 / 12825.0 * tau) f2
                 (28561.0 / 56430.0 * tau) f3 (-0.18            * tau) f4
                 (    2.0 / 55.0    * tau) f5 1.0 x
      in (x', t')
    else
      -- Error bound "eps" exceeded; we need to reduce the step and try again:
      let
        -- New step estimate:
        tau' = 0.9 * tau * (eps / err)**0.2

        -- However, we will not use "tau'" directly; better reduce the original
        -- "tau" by powers of 2 until it becomes <= "tau'":
        reduce2 ::   Double -> Double
        reduce2 dt = if dt  <= tau' then dt else reduce2 (0.5 * dt)

        tau'' = reduce2 tau
      in
        -- Retry with a reduced step ("tau''"):
        rkf5L f x t (t + tau'') eps

-------------------------------------------------------------------------------
-- "integrateODEs":                                                          --
-------------------------------------------------------------------------------
data TimeStepperMethod = RKF5

integrateODEsL ::
  ([Double] -> Double -> [Double]) -> [Double] -> Double -> Double ->
   -- f                               x0          t0        t1
   TimeStepperMethod ->   Double   -> Double   -> [Double]
   -- method              tau0        eps         x1

integrateODEsL f x0 t0 t1 method tau0 eps
  | signum (t1 - t0) /= signum tau0 = error "Invalid step"
  | otherwise                       = runStepper x0 t0 tau0
  where
  stepper :: TimeStepperL
  stepper =
    case method of
      RKF5 -> rkf5L

  runStepper :: [Double] -> Double -> Double -> [Double]
  runStepper x t tau   =
    -- Check the termination cond (here exact equality is safe):
    if   t == t1
    then x
    else
    let
      -- Compute the next time node, possibly adjusting the time step:
      t' =
        -- If with step "tau" we would over-shoot or slightly under-shoot (say
        -- by no more than 1%) the "t1" boundary, then move to "t1" exactly:
        if   abs tau >= 0.99 * abs (t1 - t)
        then t1
        else t + tau

      -- Make one step; the resulting "t''" may actually be nearer to "t" than
      -- the projected "t'":
      (x', t'') = stepper f x t t' eps

      -- The actual time step made (it will be recommended for the next iter):
      tau''     = t'' - t
    in
      -- Next iteration:
      runStepper x' t'' tau''

-------------------------------------------------------------------------------
-- Linear Combinations of Vectors:                                           --
-------------------------------------------------------------------------------
-- Assuming that all vectors are of the same length:
-- XXX: NOT using tail recursion, otherwise we would need to reverse the result:
--
lc2 :: Double -> [Double] -> Double -> [Double] -> [Double]
lc2    a0 (v0:v0s) a1 (v1:v1s) = (a0 * v0 + a1 * v1) : (lc2 a0 v0s a1 v1s)
lc2    _ _ _ _                 = []

lc3 :: Double -> [Double] -> Double -> [Double] ->
       Double -> [Double] -> [Double]
lc3    a0 (v0:v0s) a1 (v1:v1s) a2 (v2:v2s) =
       (a0 * v0 + a1 * v1 + a2 * v2) : (lc3 a0 v0s a1 v1s a2 v2s)
lc3    _ _ _ _ _ _             = []

lc4 :: Double -> [Double] -> Double -> [Double] ->
       Double -> [Double] -> Double -> [Double] -> [Double]
lc4    a0 (v0:v0s) a1 (v1:v1s) a2 (v2:v2s) a3 (v3:v3s) =
       (a0 * v0 + a1 * v1 + a2 * v2 + a3 * v3) :
       (lc4 a0 v0s a1 v1s   a2 v2s a3 v3s)
lc4    _ _ _ _ _ _ _ _         = []

lc5 :: Double -> [Double] -> Double -> [Double] ->
       Double -> [Double] -> Double -> [Double] ->
       Double -> [Double] -> [Double]
lc5    a0 (v0:v0s) a1 (v1:v1s) a2 (v2:v2s) a3 (v3:v3s) a4 (v4:v4s) =
       (a0 * v0 + a1 * v1 + a2 * v2 + a3 * v3 + a4 * v4) :
       (lc5 a0 v0s a1 v1s   a2 v2s a3 v3s a4 v4s)
lc5    _ _ _ _ _ _ _ _ _ _     = []

lc6 :: Double -> [Double] -> Double -> [Double] ->
       Double -> [Double] -> Double -> [Double] ->
       Double -> [Double] -> Double -> [Double] -> [Double]
lc6    a0 (v0:v0s) a1 (v1:v1s) a2 (v2:v2s) a3 (v3:v3s) a4 (v4:v4s) a5 (v5:v5s) =
       (a0 * v0 + a1 * v1 + a2 * v2 + a3 * v3 + a4 * v4 + a5 * v5) :
       (lc6  a0 v0s a1 v1s  a2 v2s a3 v3s a4 v4s a5 v5s)
lc6    _ _ _ _ _ _ _ _ _ _ _ _ = []

------------------------------------------------------------------------------
-- "c0norm":                                                                --
------------------------------------------------------------------------------
-- XXX: Again, NOT using tail recursion:
--
c0norm :: [Double] -> Double
c0norm (v:vs) = max (abs v) (c0norm vs)
c0norm []     = 0.0
