-- vim:ts=2:et
-- ========================================================================= --
--                                  "Grids.hs":                              --
--                   Finite Difference Parabolic PDE Solvers                 --
-- ========================================================================= --
module Grids
(
  StencilType(..), TimePropMethod(..), GridNumEnv1D(..), GridPricer1D,
  gridPricer1D
)
where

import qualified Common
import qualified Diffusions
import qualified Contracts
import qualified OptPricer
import qualified TimeSteppers

-------------------------------------------------------------------------------
-- "GridPricer1D": Grid (Finite Difference)-Based Pricer Type for 1D Diffs:  --
-------------------------------------------------------------------------------
data StencilType     =  Stencil3  | Stencil5
data TimePropMethod  =  Expl_RKF5

data GridNumEnv1D =
  GridNumEnv1D
  {
    m_stencilType    :: StencilType,
    m_ns             :: Int,         -- Number of "S" intervals (>= 1)
    m_boundsStdDevs  :: Double,      -- For construction of the Bounding Box
    m_timePropMethod :: TimePropMethod,
    m_timeStepY      :: Double,      -- Time Step in Years
    m_eps            :: Double       -- Abs accuracy of each Time Step
  }

type GridPricer1D =
  OptPricer.Pricer
    Diffusions.Diff1D OptPricer.FwdIRModel OptPricer.FwdDivsModel GridNumEnv1D

-------------------------------------------------------------------------------
-- "gridPricer1D": "GridPricer1D" Instance:                                  --
-------------------------------------------------------------------------------
gridPricer1D :: GridPricer1D
gridPricer1D diff irModel divsModel numEnv optSpec s t
  | onFut && (not zeroDivs) =
    error  "gridPricer1D: Dividents must be Const0 on Futures"
  | t >  expTime            =
      error ("mcPricer1D: curr" ++ (show t) ++ " is beyond exp" ++
            (show expTime))
  | argType  == Contracts.IntegralAvg = Nothing
    -- Not an error, but not supported by this Pricer (would need a 2D one):
  | loKnockIn || upKnockIn            = Nothing
    -- Knock-in barriers are not supported (yet)
  | otherwise =
      -- GENERIC CASE: Run the actual Grid (BI) Pricer:
      Just (gridOptPx1D diff irModel divsModel numEnv optSpec s t)
  where
  onFut    :: Bool
  onFut    =  Contracts.isOnFutures optSpec

  zeroDivs :: Bool
  zeroDivs =  (Common.getConstTF divsModel) == 0.0

  expTime  :: Common.Time
  expTime  =  Contracts.expirationTime  optSpec

  argType  :: Contracts.PayOffArgType
  argType  =  Contracts.m_payOffArgType optSpec

  loKnockIn, upKnockIn :: Bool
  loKnockIn  =
    case Contracts.m_loBarrier optSpec of
      Contracts.KnockIn _ -> True
      _                   -> False
  upKnockIn  =
    case Contracts.m_upBarrier optSpec of
      Contracts.KnockIn _ -> True
      _                   -> False

-------------------------------------------------------------------------------
-- "gridOptPx1D": Actual 1D Grid-Based Pricer (BSM PDE Solver):              --
-------------------------------------------------------------------------------
gridOptPx1D ::
  Diffusions.Diff1D -> OptPricer.FwdIRModel -> OptPricer.FwdDivsModel   ->
  GridNumEnv1D      -> Contracts.OptionSpec -> Common.Px -> Common.Time ->
  Common.Px

gridOptPx1D diff irModel divsModel numEnv optSpec
  currPx@(Common.Px st)  pricingTime
  | s0' >= sn' =
      error ("LoBound=" ++ (show s0') ++ ", UpBound=" ++ (show sn'))
  | hasLoBarr && (st <= s0') = Common.Px 0.0
  | hasUpBarr && (st >= sn') = Common.Px 0.0
  | st < s0'   =
      error ("St=" ++ (show st) ++ ", LoBound=" ++ (show s0'))
  | st > sn'   =
      error ("St=" ++ (show st) ++ ", UpBound=" ++ (show sn'))
  | otherwise                =
    Common.Px (head prevC)
  where
  -- "ty": Time to Expiration in Years:
  t, expT, ty  :: Double
  t    =   Common.m_getTimeY  pricingTime
  expT =   Common.m_getTimeY  (Contracts.expirationTime optSpec)
  ty   =   Common.assert (t <= expT) (expT - t)

  -- Construct the Boundaries:
  nStdDevs :: Double
  nStdDevs =  m_boundsStdDevs numEnv

  nonNeg, onFut :: Bool
  nonNeg   =  Diffusions.isNonNeg   diff
  onFut    =  Contracts.isOnFutures optSpec

  r, d, rdiff, sigma :: Double
  r        = Common.evalTFunc irModel      pricingTime
  d        = Common.evalTFunc divsModel    pricingTime
  sigma    = (Diffusions.getSigma1D diff)  currPx pricingTime
  rdiff    = if onFut then 0.0 else r - d

  -- s0, sn : Grid Boundaries:
  s0, sn   :: Double
  (s0, sn) =
    if nonNeg
    then
      -- Non-Negative Diffusion:
      -- Here we apply a Log-transform to S and use the Frozen Coeffs Method
      -- (freezing them at (s,t)):
      let
        sigmaN, muN, nSigmaT :: Double
        sigmaN   = Common.assert (st >  0.0)     (sigma  /  st)
        muN      = (rdiff   -    0.5 *  sigmaN *  sigmaN) * ty
        nSigmaT  = Common.assert (ty >= 0.0) (nStdDevs * sigmaN * (sqrt ty))
      in
        (st * exp (muN - nSigmaT), st * exp (muN + nSigmaT))
    else
      -- General Diffusion (possibly negative):
      -- Apply the Frozen Coeffs Method without any transforms:
      let
        mu       = rdiff *  st * ty
        nSigmaT  = nStdDevs    * sigma * (sqrt ty)
      in
        (st + mu - nSigmaT, st + mu + nSigmaT)

  -- However, if Barriers are specified, they must coincide with the boundaries
  -- of the bounding box:
  s0',       sn'       :: Double
  hasLoBarr, hasUpBarr :: Bool
  (s0', hasLoBarr) =
    case Contracts.m_loBarrier optSpec of
      Contracts.KnockOut lo -> (Common.m_getPxVal lo, True)
      _                     -> (s0, False)
  (sn', hasUpBarr) =
    case Contracts.m_upBarrier optSpec of
      Contracts.KnockOut up -> (Common.m_getPxVal up, True)
      _                     -> (sn, False)
  -- "h": Step wrt "S":
  n   :: Int
  n   =  m_ns numEnv
  h   :: Double
  h   =  Common.assert (s0' < sn' && n >= 1) ((sn' - s0') / (fromIntegral n))

  -- Construct the "S" Grid, XXX: as a List; we make sure that s0' and sn' are
  -- exactly on the grid, irrespective any rounding errors:
  sGrid  :: [Double]
  sGrid  =  s0' : [ sn' - h * (fromIntegral (n - i)) | i <- [1..n] ]

  -- Initial condition on the Grid, as a List:
  payOff :: Contracts.PayOffFunc
  payOff =  Contracts.m_payOffFunc optSpec

  prevC  :: [Double]
  prevC  =
    map
      (Common.m_getPxVal . (Contracts.evalPayOffFunc payOff) . Common.Px)
      sGrid

  -- Now run the "prevC" through a TimeStepper:

