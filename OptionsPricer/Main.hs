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
-- For numerical integration:
nStdDevs = 6
bsNumEnv = OptPricer.BSMNumEnv { OptPricer.m_nStdDevs = nStdDevs }

-- BSM Price the option:
runBSM ::  String -> Common.Px
runBSM     optSpecStr =
  OptPricer.bsmPricer   diff rTF divsTF bsNumEnv (read optSpecStr) s t

runMC ::   String -> String -> Common.Px
runMC      optSpecStr mcNumEnvStr =
  MonteCarlo.mcPricer1D diff rTF divsTF (read mcNumEnvStr) (read optSpecStr) s t

main :: IO ()
main =
  do
    mcNumEnvStr <- getLine
    optSpecStr  <- getLine
    putStrLn ("BSM: " ++ (show (runBSM optSpecStr)))
    putStrLn ("MC : " ++ (show (runMC  optSpecStr mcNumEnvStr)))

