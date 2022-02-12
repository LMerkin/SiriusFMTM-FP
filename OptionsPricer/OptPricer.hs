-- vim:ts=2:et
-- ========================================================================= --
--                                "OptPricer.hs":                            --
--                        Options Pricer: Generic and BSM                    --
-- ========================================================================= --
module OptPricer
(
  Pricer,
  BSMPricer, BSMNumEnv(..), bsmPricer, FwdIRModel, FwdDivsModel
)
where

import qualified Common
import qualified Diffusions
import qualified Contracts
import qualified Data.Number.Erf
import qualified GaussianQuadratureIntegration

-------------------------------------------------------------------------------
-- General Option Pricer Type:                                               --
-------------------------------------------------------------------------------
type Pricer diff irModel divsModel numEnv =
  diff                 ->     -- Diffusion (Model) of the Underlying Px
  irModel              ->     -- Interest Rates Model (eg fwd curve)
  divsModel            ->     -- Dividends  Model
  numEnv               ->     -- Numerical Environment
  Contracts.OptionSpec ->     -- Strike and ExpirTime are here
  Common.Px            ->     -- Curr Underlying Px
  Common.Time          ->     -- Curr Time
  Common.Px                   -- OptionPx

-------------------------------------------------------------------------------
-- Ineterest Rates and Dividend Rates Models as Fwd Curves:                  --
-------------------------------------------------------------------------------
-- FIXME! Assuming continuously-payable dividends only for the moment, more app-
-- licable to FX -- then it is the IR model for the Base Ccy (aka CcyA):
--
type FwdIRModel   = Common.TFunc  -- Fwd IR Curve
type FwdDivsModel = Common.TFunc  -- Fwd Divs Rate Curve

-------------------------------------------------------------------------------
-- "BSMPricer": Black-Scholes-Merton (BSM) Pricer Type:                      --
-------------------------------------------------------------------------------
-- BSM Numerical Environment:
-- Only required when we perform numerical integration of Green's Functions:
--
data BSMNumEnv   = BSMNumEnv { m_nStdDevs :: Double }
dummyBSMNumEnv   = BSMNumEnv { m_nStdDevs =  0.0 }

-- It requires a "Diff1D":
type BSMPricer   = Pricer Diffusions.Diff1D FwdIRModel FwdDivsModel BSMNumEnv

-------------------------------------------------------------------------------
-- "bsmPricer": "BSMPricer" Instance:                                        --
-------------------------------------------------------------------------------
bsmPricer ::  BSMPricer
bsmPricer     diff irModel divsModel numEnv optSpec s t
  | Contracts.m_loBarrier     optSpec /= Contracts.NoBarr ||
    Contracts.m_upBarrier     optSpec /= Contracts.NoBarr   =
      error  "bsmPricer: Barriers are not supported"
  | Contracts.m_payOffArgType optSpec /= Contracts.FinalEuropean =
      error  "bsmPricer: European PayOffArgType is required"
  | onFut && (not zeroDivs)                    =
      error  "bsmPricer: Dividents must be Const0 on Futures"
  | t < Contracts.m_created optSpec =
      error ("bsmPricer: created" ++ (show (Contracts.m_created optSpec)) ++
             " is below curr"     ++ (show t))
  | t >  expTime =
      -- This is unsupported yet:
      error ("bsmPricer: curr" ++ (show t) ++ " is beyond exp" ++
            (show expTime))
  | t == expTime =
      -- Just evaluate the PayOff at the curr "s":
      Contracts.evalPayOffFunc payOffFunc s
  | otherwise                                  =
      bsmOptPx payOffFunc onFut expTime vol irModel divsModel numEnv s t

  where
  -- Underlying Type: Futures or Spot? (XXX: Options on Options are not yet
  -- supported):
  onFut    :: Bool
  onFut    =  Contracts.isOnFutures optSpec
  zeroDivs :: Bool
  zeroDivs = (Common.getConstTF divsModel) == 0.0

  -- Exercise Time must be European, same as Expiration Time:
  expTime :: Common.Time
  expTime =
    case Contracts.m_exerciseTimes optSpec of
      Contracts.European t -> t
      _ -> error "bsmPricer: European Exercise is required"

  -- GBM vol:
  vol :: Common.TFunc
  vol =
    case (Diffusions.getVolType1D diff) of
      Diffusions.GBM vol' -> vol'
      _ -> error "bsmPricer: GBM Model is required"

  -- PayOffFunc:
  payOffFunc :: Contracts.PayOffFunc
  payOffFunc =  Contracts.m_payOffFunc optSpec

-------------------------------------------------------------------------------
-- "bsmOptPx": BSM Pricer Implementation:                                    --
-------------------------------------------------------------------------------
--          payOff                  OnFut?  expTime
bsmOptPx :: Contracts.PayOffFunc -> Bool -> Common.Time  ->
--          vol                     rTF
            Common.TFunc     -> Common.TFunc         ->
--          divsTF                  numEnv
            Common.TFunc     -> BSMNumEnv                ->
--          currS                   currT                       optionPx
            Common.Px        -> Common.Time          -> Common.Px
--
-- Call:
--
bsmOptPx   (Contracts.Call (Common.Px strike)) onFut expT volTF rTF divsTF
           _ (Common.Px currS)  currT =
  Common.Px optPx
  where
  -- Quandratic Variance as function of time:
  varTF :: Common.TFunc
  varTF =  Common.mapTFunc (\sigma -> sigma * sigma) volTF

  -- Integrated Quadratic Variance:
  var   :: Double
  var  =   Common.integrateTFunc varTF currT expT

  -- Effective Standard Deviation: (sigma * sqrt(tau)):
  std   :: Double
  std  =   sqrt var

  -- Integrated Interest Rate:
  rInt  :: Double
  rInt  =  Common.integrateTFunc rTF    currT expT

  -- Discount Factor wrt "r" (or "rB" for FX):
  rDF   :: Double
  rDF   =  exp (- rInt)

  -- Integrated Dividends Rate:
  dInt  :: Double
  dInt  =  Common.integrateTFunc divsTF currT expT

  -- Discount Factor wrt "d" (or "rA" for FX):
  dDF   :: Double
  dDF   =  exp(- dInt)

  -- Moneyness:
  m     :: Double
  m     =  Common.assert (strike > 0 && currS > 0) (log (currS / strike))

  -- First Term:
  n1arg :: Double
  n1arg =  (m + (if onFut then 0.0 else rInt - dInt)) / std + 0.5 * std
  n1    :: Double
  n1    =  Data.Number.Erf.normcdf n1arg

  -- Second Term:
  n2arg :: Double
  n2arg =  n1arg - std
  n2    :: Double
  n2    =  Data.Number.Erf.normcdf n2arg

  -- The result:
  optPx :: Double
  optPx =  dDF * currS * n1 - rDF * strike * n2

--
-- Put:
--
bsmOptPx   (Contracts.Put k@(Common.Px strike)) onFut expT volTF rTF divsTF
           _ s@(Common.Px currS) currT =
  Common.Px (callPx  - fwdVal)
  where
  callPx :: Double
  Common.Px callPx =
    bsmOptPx (Contracts.Call k) onFut expT volTF rTF divsTF
             dummyBSMNumEnv  s  currT

  fwdVal :: Double
  fwdVal =  currS - strike * exp (- (Common.integrateTFunc rTF currT expT))

--
-- Any Linear Combination of Other PayOffs:
--
bsmOptPx (Contracts.LinearC ls) onFut expT volTF rTF divsTF numEnv currS currT =
  Common.Px sumPx
  where
  sumPx :: Double
  sumPx =
    foldl
      (\currSum  (coeff, payOff) ->
       let
         Common.Px px =
          bsmOptPx payOff onFut expT volTF rTF divsTF numEnv currS currT
       in
         currSum + coeff * px
      )
      0.0 ls

--
-- Arbitrary PayOff:
--
bsmOptPx (Contracts.AnyPOF phi) onFut expT volTF rTF divsTF numEnv
  (Common.Px currS)  currT =
  Common.Px  optPx
  where
  -- Quandratic Variance as function of time:
  varTF :: Common.TFunc
  varTF =  Common.mapTFunc (\sigma -> sigma * sigma) volTF

  -- Integrated Quadratic Variance, halved (aka tau(t..T)):
  tau   :: Double
  tau  =   0.5 * Common.integrateTFunc varTF currT expT

  -- Integrated Interest Rate:
  rInt  :: Double
  rInt  =  Common.integrateTFunc rTF    currT expT

  -- Discount Factor wrt "r" (or "rB" for FX):
  rDF   :: Double
  rDF   =  exp (- rInt)

  -- Integrated Dividends Rate:
  dInt  :: Double
  dInt  =  Common.integrateTFunc divsTF currT expT

  -- Integrated         beta = r - d - sigma^2/2
  -- if "onFut" is set, beta = - sigma^2/2:
  betaInt  :: Double
  betaInt  =  if onFut then (- tau) else rInt - dInt - tau

  -- Standard Deviation of the Gaussian Kernel:
  stdDev :: Double
  stdDev =  sqrt (2.0 * tau)

  -- Center of the integration inteval:
  m :: Double
  m =  Common.assert (currS > 0.0) (log currS + betaInt)

  -- Half-size of the integration interval:
  h :: Double
  h =  (m_nStdDevs numEnv) * stdDev

  -- Integration limits:
  a :: Double
  b :: Double
  a = m - h
  b = m + h

  -- Function to be integrated:
  integrand :: Double -> Double
  integrand x =
    let Common.Px payOff = phi (Common.Px (exp x))
    in  payOff * exp (- (m - x)**2 / (4.0 * tau))

  -- Integrate using the Gauss-Legendre method:
  intGL :: Double
  intGL =  GaussianQuadratureIntegration.nIntegrate1024 integrand a b

  -- Option Px:
  optPx :: Double
  optPx =  rDF / (2.0 * sqrt (pi *  tau)) * intGL

