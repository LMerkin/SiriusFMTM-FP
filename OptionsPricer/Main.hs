-- vim:ts=2:et
-- ========================================================================= --
--                                   "Main.hs":                              --
-- ========================================================================= --
module Main
where

import qualified Common
import qualified Diffusions
import qualified Contracts
import qualified OptPricer
import qualified MonteCarlo
import qualified System.Random

-- Call Option:
strike   = 85.0
expT     = Common.mkTimeY 2023.1
-- For numerical integration:
nStdDevs = 6

-- With PayOff as a lambda:
payOffAny  =
  Contracts.AnyPOF (\(Common.Px s) -> Common.Px (max (s-strike) 0))

-- With same symbolic PayOff:
payOffSym  = Contracts.Call (Common.Px strike)

-- All together as "OptionSpec"s:
optSpecAny =
  Contracts.OptionSpec
  {
    Contracts.m_underlying    = Contracts.Spot "USD/RUB",
    Contracts.m_created       = t,
    Contracts.m_payOffFunc    = payOffAny,
    Contracts.m_payOffArgType = Contracts.FinalEuropean,
    Contracts.m_exerciseTimes = Contracts.European expT,
    Contracts.m_loBarrier     = Contracts.NoBarr,
    Contracts.m_upBarrier     = Contracts.NoBarr
  }
optSpecSym = optSpecAny { Contracts.m_payOffFunc = payOffSym }

-- Interest Rate and Dividend Rate:
rTF    = Common.mkConstTF 0
divsTF = Common.mkConstTF 0

-- Curr underlying px and curr time:
s = Common.Px      77.0
t = Common.mkTimeY 2022.1

-- Diffusion: GBM with the givel "volTF"; trend ("mu") does not matter here --
-- the risk-neutral trend is automatically selected:
volTF = Common.mkConstTF 0.25
diff  = Diffusions.mkGBM (Common.mkConstTF 0.0) volTF

-- BSM Numerical Environment:
bsNumEnv = OptPricer.BSMNumEnv { OptPricer.m_nStdDevs = nStdDevs }

-- BSM Price the option using the Lambda PayOff:
pxBSAny = OptPricer.bsmPricer diff rTF divsTF bsNumEnv optSpecAny s t

-- BSM Price the option  using the symbolic PayOff:
pxBSSym = OptPricer.bsmPricer diff rTF divsTF bsNumEnv optSpecSym s t

-- Monte-Carlo Numerical Environment and the Initial State:
mcNumEnv =
  MonteCarlo.MCNumEnv1D
  {
    MonteCarlo.m_nPaths     = 100000,
    MonteCarlo.m_timeStepY  = 0.001,
    MonteCarlo.m_rngSeed    = 12345,
    MonteCarlo.m_nParBlocks = 24
  }
pxMCSym = MonteCarlo.mcPricer1D diff rTF divsTF mcNumEnv optSpecSym s t

main :: IO ()
main =
  do
    putStrLn ("BSM Sym: " ++ (show pxBSSym))
    putStrLn ("BSM Lbd: " ++ (show pxBSAny))
    putStrLn ("MC  Sym: " ++ (show pxMCSym))

