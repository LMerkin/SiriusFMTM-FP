-- vim:ts=2:et
-- ========================================================================= --
--                         "FwdCurvesCalibrator.hs":                         --
--       Construction of Forward IR Curves via the Optimisation Method       --
-- ========================================================================= --
module FwdCurvesCalibrator
(
  FwdCurves(..), pxIRP, mkClassicalIRS
)
where

import qualified Common
import qualified Contracts

-------------------------------------------------------------------------------
-- "pxFwdLIBOR": Similar to LIBOR, but with a Fwd start:                     --
-------------------------------------------------------------------------------
--            StartDate      LIBOR Tenor        GOVT Curve
pxFwdLIBOR :: Common.Date -> Contracts.Tenor -> Common.TFunc -> Double ->
--            FwdLIBOR       grad=[dL/dIcepts  dL/dSlopes  dL/dParShitf]
              (Double, [Double])

pxFwdLIBOR    startDate tenor govtCurve parShift =
  -- FwdLIBOR definition:
  -- (delta * fwdLIBOR + 1) ==
  -- AntiDF(startDate, startDate+tenor; govtCurve + parShift)
  let
    -- Verify that startTime is no earlietr than pxTime, where the latter is
    -- taken as the earlist time in the GOVT curve under construction:
    pxTime :: Common.Time
    pxTime =  Common.minTime govtCurve
    --
    -- Start and Expiration Time (XXX: here dayFracs are computed using the
    -- 11 am UTC convention):
    dayFrac :: Double
    dayFrac =  11.0 / 24.0

    t0, tE  :: Common.Time
    t0  = Common.mkTimeD startDate  dayFrac
    tE  = Common.mkTimeD (Contracts.addTenor startDate tenor) dayFrac

    -- delta is the year fraction for the tenor:
    delta   :: Double
    delta = (Common.getTimeY tE) - (Common.getTimeY t0)

    -- Integrate the GOVT curve; "iceptDs" and "slopeDs" are derivatives of
    -- "intGov" wrt to Intercept and Slope coeffs:
    intGov   :: Double
    iceptDs  :: [Double]
    slopeDs  :: [Double]
    (intGov, iceptDs, slopeDs) =
      Common.assert (pxTime <= t0 && delta > 0.0)
                    (Common.integrateTFunc govtCurve t0 tE)

    -- Integrate the LIBOR curve: it has a fixed offset (parShift) to the
    -- GOVT curve:
    intLIBOR :: Double
    intLIBOR =  intGov + parShift * delta

    -- AntiDF and LIBOR:
    antiDF, libor    :: Double
    antiDF   =  exp intLIBOR
    libor    =  (antiDF - 1) / delta

    -- Gradient as a list of 1st derivatives:
    -- XXX: sub-optimal, will be changed later:
    grad     :: [Double]
    grad     =  [antiDF / delta * iceptD | iceptD <- iceptDs] ++
                [antiDF / delta * slopeD | slopeD <- slopeDs] ++ [antiDF]
  in
    (libor, grad)

-------------------------------------------------------------------------------
-- "pxFwdClassicalIRS":                                                      --
-------------------------------------------------------------------------------

-------------------------------------------------------------------------------
-- "FwdCurves": Data for GOVT and 3mLIBOR Fwd Curves:                        --
-------------------------------------------------------------------------------
--                         GOVT Curve   ParShift of 3mLIBOR Curve over GOVT
data FwdCurves = FwdCurves Common.TFunc Double

-------------------------------------------------------------------------------
-- "pxIRP":                                                                  --
-------------------------------------------------------------------------------
-- Top-level function for pricing ANY interest-rate products using given
-- FwdCurves:
-- Returns (px, grad = pxDerivatives_wrt_icepts,slopes,parShift):
--
pxIRP :: Contracts.IRP -> FwdCurves -> (Double, [Double])

pxIRP (Contracts.FwdLIBOR startDate tenor) (FwdCurves govtCurve parShift) =
  pxFwdLIBOR startDate tenor govtCurve parShift

pxIRP (Contracts.FwdClassicalIRS   qSched  (FwdCurves govtCurve parShift) =
  pxFwdClassicalIRS   qSched govtCurve parShift

-------------------------------------------------------------------------------
-- "mkClassicalIRS":                                                         --
-------------------------------------------------------------------------------
mkClassicalIRS :: Common.Date -> Contracts.Tenor -> Contracts.IRP
mkClassicalIRS    startDate swapTenor =
  let
    -- Construct a quarterly schedule, beginning with "startDate":
    -- The total number of Quarters:
    nQuarters :: Int
    nQuarters =
      case swapTenor of
        Contracts.Y1  ->  4
        Contracts.Y2  ->  8
        Contracts.Y3  -> 12
        Contracts.Y5  -> 20
        Contracts.Y7  -> 28
        Contracts.Y10 -> 40

    -- Tail-recursive Quarters generator:
    mkQuarters :: Int -> Common.Date -> [Common.Date] -> [Common.Date]
    mkQuarters    remQs  qStartDate     currQs        =
      if remQs <= 0
      then currQs
      else mkQuarters (remQs-1) (Contracts.addTenor qStartDate Contracts.M3)
                      (qStartDate : currQs)
  in
    -- Run the generator:
    Contracts.ClassicalIRS (mkQuarters nQuarters startDate [])

