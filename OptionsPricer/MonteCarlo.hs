-- vim:ts=2:et
-- ========================================================================= --
--                               "MonteCarlo.hs":                            --
--                  Options Pricer using Monte-Carlo Methods                 --
-- ========================================================================= --
module MonteCarlo
where

import qualified Common
import qualified Diffusions
import qualified Contracts
import qualified OptPricer

-------------------------------------------------------------------------------
-- "MCPricer1D": Monte-Carlo Pricer Type for 1D Diffusions:                  --
-------------------------------------------------------------------------------
data MCNumEnv1D =
  MCNumEnv1D
  {
    m_nPaths    :: Double,    -- Number   of Monte-Carlo paths to be generated
    m_timeStepY :: Double     -- TimeStep in Years (e.g. 1e-3 or 1e-4)
  }

-- For "MCPricer1D": Diffusion must be "Diff1D", and IR and Divs models are to
-- be FwdCurves:
type MCPricer1D =
  OptPricer.Pricer
    Diffusions.Diff1D OptPricer.FwdIRModel OptPricer.FwdDivsModel MCNumEnv1D

-------------------------------------------------------------------------------
-- "mcPricer1D": "MCPricer1D" Instance:                                      --
-------------------------------------------------------------------------------
mcPricer1D :: MCPricer1D
mcPricer1D diff irModel divsModel numEnv optSpec s t
  | onFut && (not zeroDivs)                    =
      error  "mcPricer1D: Dividents must be Const0 on Futures"
  | t >  expTime =
      -- This is unsupported yet:
      error ("mcPricer1D: curr" ++ (show t) ++ " is beyond exp" ++
            (show expTime))
  | otherwise =
      mcOptPx1D diff irModel divsModel numEnv optSpec s t
  where
  -- Underlying Type: Futures or Spot? (XXX: Options on Options are not yet
  -- supported):
  onFut    :: Bool
  onFut    =  Contracts.isOnFutures optSpec
  zeroDivs :: Bool
  zeroDivs = (Common.getConstTF divsModel) == 0.0

  -- Exercise Time must be European, same as Expiration Time. This is because
  -- the curr Monte-Carlo method is unsuitable for pricing American or Bermudan
  -- options:
  expTime :: Common.Time
  expTime =
    case Contracts.m_exerciseTimes optSpec of
      Contracts.European t -> t
      _ -> error "mcPricer1D: European Exercise is required"

-------------------------------------------------------------------------------
-- "mcOptPx1D":                                                              --
-------------------------------------------------------------------------------
