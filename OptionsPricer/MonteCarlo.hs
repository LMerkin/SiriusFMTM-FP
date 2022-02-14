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
import qualified System.Random
import qualified Data.Word
import           Control.Parallel

-------------------------------------------------------------------------------
-- "MCPricer1D": Monte-Carlo Pricer Type for 1D Diffusions:                  --
-------------------------------------------------------------------------------
data MCNumEnv1D =
  MCNumEnv1D
  {
    m_nPaths     :: Int,    -- Number   of Monte-Carlo paths to be generated
    m_timeStepY  :: Double, -- TimeStep in Years (e.g. 1e-3 or 1e-4)
    m_rngSeed    :: Int,    -- To initialise the Random Number Generator
    m_nParBlocks :: Int     -- Number of conceptually-parallel blocks
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
      -- GENERIC CASE:
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
-- "mcOptPx1D": Driver for the Monte-Carlo Paths Evaluation:                 --
-------------------------------------------------------------------------------
mcOptPx1D ::
  Diffusions.Diff1D -> OptPricer.FwdIRModel -> OptPricer.FwdDivsModel   ->
  MCNumEnv1D        -> Contracts.OptionSpec -> Common.Px -> Common.Time ->
  Common.Px

mcOptPx1D diff irModel divsModel numEnv optSpec s t = Common.Px discExpPayOff
  where
  -- The actual NPaths will be a multiple of 4:
  nPaths  :: Int
  nPaths  =  m_nPaths numEnv

  -- Number of Path Quadrupes (each of them is produced by one run of
  -- "mcEvalPaths4"):
  nQuadrs :: Int
  nQuadrs =
    if nPaths `mod` 4 == 0 then nPaths `div` 4 else (nPaths `div` 4) + 1

  -- FwdCurves are provided if the Option is NOT on Futures:
  mbFwdCurves :: Maybe (OptPricer.FwdIRModel, OptPricer.FwdDivsModel)
  mbFwdCurves =
    if   Contracts.isOnFutures optSpec
    then Nothing
    else Just (irModel, divsModel)

  -- Divide "nQuadrs" into (conceptually-parallel) blocks:
  blockSz, nBlocks :: Int
  nBlocks = m_nParBlocks numEnv
  blockSz =
    if   nBlocks <= 0
    then error "mcOptPx1D: nParBlocks must be >= 1"
    else
      if    nQuadrs `mod` nBlocks == 0
      then  nQuadrs `div` nBlocks
      else (nQuadrs `div` nBlocks) + 1

  -- List of all Block IDs:
  blockIDs :: [Int]
  blockIDs =  [0 .. (nBlocks-1)]

  -- PayOff Sums for all blocks:
  bSums    :: [Double]
  bSums    =
    map (\bID -> mcEvalBlock diff mbFwdCurves optSpec numEnv bID blockSz s t)
        blockIDs

  -- Evaluate "bSums", conceptually in parallel:
  parList  :: [Double] -> Double -> Double
  parList     []          b      =  b
  parList     (x:xs)      b      =  x `par` (parList xs b)

  -- The Avg (Expected) PayOff over all Blocks:
  expPayOff  :: Double
  expPayOff  =  (parList bSums (sum bSums)) / (4.0 * fromIntegral nQuadrs)

  -- Option Expiration Time:
  tExp  :: Common.Time
  tExp  =  Contracts.expirationTime optSpec

  -- Integrated Interest Rate and Discount Factor (t .. expT):
  rInt  :: Double
  rInt  =  Common.integrateTFunc irModel t tExp

  -- Discount Factor wrt "r" (or "rB" for FX):
  rDF   :: Double
  rDF   =  exp (- rInt)

  -- Finally, discount the Expected PayOff:
  discExpPayOff :: Double
  discExpPayOff =  rDF * expPayOff

-------------------------------------------------------------------------------
-- Internal State of the MC Path Generator:                                  --
-------------------------------------------------------------------------------
data MCPathGenState1D =
  MCPathGenState1D
  {
    -- Technical:
    m_rngState :: System.Random.StdGen, -- State of the Random Number Generator

    -- Paths Generation:
    -- Curr "S(t)" vals along 4 simultaneously-generated paths, and "t":
    m_St0      :: Common.Px,
    m_St1      :: Common.Px,
    m_St2      :: Common.Px,
    m_St3      :: Common.Px,
    m_t        :: Common.Time,    -- Curr time

    -- State of the Option Pricer -- for efficiency, we put it directly into the
    -- Path Generator State:
    -- Integrals of S(t) along the 4 paths (for Asian options):
    m_IS0      :: Double,
    m_IS1      :: Double,
    m_IS2      :: Double,
    m_IS3      :: Double,

    -- Knock-in OR knock-out conds for Barrier options along the 4 paths, for
    -- Lower and Upper Barriers:
    m_knockLo0 :: Bool,
    m_knockUp0 :: Bool,
    m_knockLo1 :: Bool,
    m_knockUp1 :: Bool,
    m_knockLo2 :: Bool,
    m_knockUp2 :: Bool,
    m_knockLo3 :: Bool,
    m_knockUp3 :: Bool
  }

-------------------------------------------------------------------------------
-- "initMCPathGenState1D":                                                   --
-------------------------------------------------------------------------------
-- Create the initial Path Generator State
-- Returns (InitState, ContFlag)
-- where ContFlag is False if the Option has immediately KnockedOut, or we are
-- at the ExpirationTime:
--
initMCPathGenState1D ::
  --        OptSpec      RNG
  Contracts.OptionSpec ->System.Random.StdGen ->
  --     St              t
  Common.Px           -> Common.Time          -> (MCPathGenState1D, Bool)

initMCPathGenState1D optSpec rngState s t     =  (state, cont)
  where
  -- NB: Prev conds are onviously "False" in all cases:
  (knockLo, outLo) = ckKnock optSpec False s False
  (knockUp, outUp) = ckKnock optSpec True  s False

  immedOut :: Bool
  immedOut =  outLo || outUp

  tExp     :: Common.Time
  tExp     =  Contracts.expirationTime optSpec
  cont     :: Bool
  cont     =  Common.assert (t <= tExp) (t < tExp && not immedOut)

  state    =
    MCPathGenState1D
    {
      m_rngState = rngState,
      m_St0      = s,
      m_St1      = s,
      m_St2      = s,
      m_St3      = s,
      m_t        = t,
      m_IS0      = 0,
      m_IS1      = 0,
      m_IS2      = 0,
      m_IS3      = 0,
      m_knockLo0 = knockLo,
      m_knockUp0 = knockUp,
      m_knockLo1 = knockLo,
      m_knockUp1 = knockUp,
      m_knockLo2 = knockLo,
      m_knockUp2 = knockUp,
      m_knockLo3 = knockLo,
      m_knockUp3 = knockUp
    }

-------------------------------------------------------------------------------
-- "mcStep1D":                                                               --
-------------------------------------------------------------------------------
-- XXX: We currently assume that the MC Step is performed in Risk-Neutral Msr:
-- Returns (ContinueFlag, NewState):
--
mcStep1D :: Diffusions.Diff1D      ->
            Maybe (OptPricer.FwdIRModel, OptPricer.FwdDivsModel) ->
            Contracts.OptionSpec   -> Double                     ->
            MCPathGenState1D       -> (MCPathGenState1D,   Bool)

mcStep1D diff mbFwdCurves optSpec dt state =    (state',  cont')
  where
  -- Generate a uniformly-distributed Word64:
  rngPair0 :: (Data.Word.Word64, System.Random.StdGen)
  rngPair0 =  System.Random.uniform (m_rngState state)

  rngPair1 :: (Data.Word.Word64, System.Random.StdGen)
  rngPair1 =  System.Random.uniform (snd rngPair0)

  -- Convert random Word64s into uniformly-distributed Doubles in [0..1]:
  d0, d1, max64, u0, u1 :: Double
  d0    =  fromIntegral (fst rngPair0)
  d1    =  fromIntegral (fst rngPair1)
  max64 =  fromIntegral (maxBound :: Data.Word.Word64)
  u0    =  d0 / max64
  u1    =  d1 / max64

  -- Using the Box-Muller method, produce 2 N(0,1) pseudo-random numbers:
  r, theta, z0, z1 :: Double
  r     =  sqrt  (-2.0 * log u0)
  theta =  2.0 * pi * u1
  z0    =  r * cos theta
  z1    =  r * sin theta

  -- Update the Paths:
  -- Compute the 4 Risk-Neutral Trends if required:
  -- Make sure we do not move beyond the Option Expiration Time:
  t, t', t'', tExp  ::  Common.Time
  t    = m_t state
  tExp = Contracts.expirationTime optSpec

  t'   = Common.assert (t <= tExp) (Common.mkTimeY (Common.getTimeY t + dt))
  t'' = if t' > tExp then tExp else t'

  -- This may also require adjustment of dt:
  dt' :: Double
  dt' = if t' > tExp then Common.getTimeY tExp - Common.getTimeY t else dt

  -- Continuation flag: whether this step will not arrive at the very end yet:
  cont :: Bool
  cont =  Common.assert (t'' <= tExp && dt' > 0) (t'' < tExp)

  -- "ratesDiff" is (r(t)-divs(t)) * dt', if the rates are present (NB: they
  -- are NOT present for Options on Futures):
  ratesDiff :: Double
  ratesDiff =
    (case mbFwdCurves of
      Just (fwdIRModel, fwdDivsModel) ->
        (Common.evalTFunc fwdIRModel t) - (Common.evalTFunc fwdDivsModel t)
      Nothing -> 0.0
    )
    * dt'

  -- Curr Underlyinhg Pxs along the 4 Paths:
  px0, px1, px2, px3 :: Common.Px
  px0 = m_St0 state
  px1 = m_St1 state
  px2 = m_St2 state
  px3 = m_St3 state

  -- Same pxs as  mere "Double"s:
  s0,  s1,  s2,  s3  :: Double
  Common.Px s0 = px0
  Common.Px s1 = px1
  Common.Px s2 = px2
  Common.Px s3 = px3

  dm0, dm1, dm2, dm3 :: Double
  dm0 = ratesDiff * s0
  dm1 = ratesDiff * s1
  dm2 = ratesDiff * s2
  dm3 = ratesDiff * s3

  -- Make 4 Brownian Motion increments, incl 2 AntiThetic ones:
  sdt, dW0, dW1, dW2, dW3 :: Double
  sdt   =  sqrt  dt'
  dW0   =  sdt * z0
  dW1   =  sdt * z1
  dW2   =  - dW0
  dW3   =  - dW1

  -- Diffusion's "sigma" as a Lambda:
  sigma :: Common.Px -> Common.Time -> Double
  sigma =  Diffusions.getSigma1D  diff

  -- Now the 4 Volatilities (with BM increments):
  dx0, dx1, dx2, dx3 :: Double
  dx0   = (sigma px0 t) * dW0
  dx1   = (sigma px1 t) * dW1
  dx2   = (sigma px2 t) * dW2
  dx3   = (sigma px3 t) * dW3

  -- Next "S" vals:
  s0', s1', s2', s3' :: Double
  s0'   = s0 + dm0 + dx0
  s1'   = s1 + dm1 + dx1
  s2'   = s2 + dm2 + dx2
  s3'   = s3 + dm3 + dx3

  -- Prevent crossing 0s if the Diffusion is Non-Negative, wrap the results
  -- into "Common.Px":
  isNonNeg :: Bool
  isNonNeg =  Diffusions.isNonNeg diff

  px0', px1', px2', px3' :: Common.Px
  px0'  = Common.Px (if isNonNeg && s0' < 0 then 0 else s0')
  px1'  = Common.Px (if isNonNeg && s1' < 0 then 0 else s1')
  px2'  = Common.Px (if isNonNeg && s2' < 0 then 0 else s2')
  px3'  = Common.Px (if isNonNeg && s3' < 0 then 0 else s3')

  -- Adjust Integrated Underlying Pxs if the Option requires that:
  -- XXX: TODO: Implement other types of Path Dependencies!
  isIntAvg = Contracts.m_payOffArgType optSpec == Contracts.IntegralAvg

  is0,  is1,  is2,  is3 :: Double
  is0   = m_IS0 state
  is1   = m_IS1 state
  is2   = m_IS2 state
  is3   = m_IS3 state

  -- NB: Use OLD vals (s0,...,s3) in integration here!
  is0', is1', is2', is3' :: Double
  is0'  = if isIntAvg then is0 + s0 * dt' else is0
  is1'  = if isIntAvg then is1 + s1 * dt' else is1
  is2'  = if isIntAvg then is2 + s2 * dt' else is2
  is3'  = if isIntAvg then is3 + s3 * dt' else is3

  -- Check the KnockIn / KnockOut Conds for all Paths:
  (knockLo0', outLo0) = ckKnock optSpec False px0' (m_knockLo0 state)
  (knockUp0', outUp0) = ckKnock optSpec True  px0' (m_knockUp0 state)

  (knockLo1', outLo1) = ckKnock optSpec False px1' (m_knockLo1 state)
  (knockUp1', outUp1) = ckKnock optSpec True  px1' (m_knockUp1 state)

  (knockLo2', outLo2) = ckKnock optSpec False px2' (m_knockLo2 state)
  (knockUp2', outUp2) = ckKnock optSpec True  px2' (m_knockUp2 state)

  (knockLo3', outLo3) = ckKnock optSpec False px3' (m_knockLo3 state)
  (knockUp3', outUp3) = ckKnock optSpec True  px3' (m_knockUp3 state)

  -- If all 4 paths have knocked out, there is no point in continuing, so reset
  -- the "cont" flag in that case. It is sufficient to get a KnockOut at either
  -- barrier:
  allOut :: Bool
  allOut =
    (outLo0 || outUp0) && (outLo1 || outUp1) &&
    (outLo2 || outUp2) && (outLo3 || outUp3)

  cont'  :: Bool
  cont'  =  cont && (not allOut)

  -- Update the State:
  state' :: MCPathGenState1D
  state' =
    state
    {
      m_rngState = snd rngPair1,
      m_St0      = px0',
      m_St1      = px1',
      m_St2      = px2',
      m_St3      = px3',
      m_t        = t',
      m_IS0      = is0',
      m_IS1      = is1',
      m_IS2      = is2',
      m_IS3      = is3',
      m_knockLo0 = knockLo0',
      m_knockUp0 = knockUp0',
      m_knockLo1 = knockLo1',
      m_knockUp1 = knockUp1',
      m_knockLo2 = knockLo2',
      m_knockUp2 = knockUp2',
      m_knockLo3 = knockLo3',
      m_knockUp3 = knockUp3'
    }

-------------------------------------------------------------------------------
-- "mcEvalPaths4":                                                           --
-------------------------------------------------------------------------------
-- Evaluate 4 Monte-Carlo Paths in one go:
-- Returns the sum of 4 PayOffs evaluated over those Paths:
--
mcEvalPaths4 :: Diffusions.Diff1D                                    ->
                Maybe (OptPricer.FwdIRModel, OptPricer.FwdDivsModel) ->
                Contracts.OptionSpec -> MCNumEnv1D  ->
                System.Random.StdGen -> Common.Px   -> Common.Time   ->
                (Double, System.Random.StdGen)

mcEvalPaths4 diff mbFwdCurves optSpec numEnv rngState  s t  = (sum4, rngState')
  where
  -- "state_t" is the initial state (at "t"), and continuation flag (if False,
  -- we do not even start the Paths):
  state_t :: MCPathGenState1D
  cont_t  :: Bool
  (state_t, cont_t) = initMCPathGenState1D optSpec rngState s t

  -- "state_T" is the final state at Option Expiration Time:
  state_T :: MCPathGenState1D
  state_T =  if cont_t then mcEvalPaths4' state_t else state_t

  -- NB: "tFin" may come earlier than the expiration time if the KnockOut on
  -- all 4 Paths was detected:
  tFin    :: Common.Time
  tFin    =  m_t state_T

  -- Normal TimeStep (may be smaller for the final step):
  dt      :: Double
  dt      = m_timeStepY  numEnv

  -- Run a tail recursion until the "cont" flag  is reset:
  -- NB: The "rngState" is threaded through "state"s here:
  mcEvalPaths4' :: MCPathGenState1D -> MCPathGenState1D
  mcEvalPaths4'  state =
    let (state', cont) = mcStep1D diff mbFwdCurves optSpec dt state
    in
      if cont
      then mcEvalPaths4' state'
      else state'

  -- Analyse the "state_T": compute the sum of the 4 PayOffs:
  po0, po1, po2, po3, sum4 :: Double

  Common.Px po0 =
    evalPayOff optSpec t (m_St0      state_T) (m_IS0      state_T) tFin
                         (m_knockLo0 state_T) (m_knockUp0 state_T)
  Common.Px po1 =
    evalPayOff optSpec t (m_St1      state_T) (m_IS1      state_T) tFin
                         (m_knockLo1 state_T) (m_knockUp1 state_T)
  Common.Px po2 =
    evalPayOff optSpec t (m_St2      state_T) (m_IS2      state_T) tFin
                         (m_knockLo2 state_T) (m_knockUp2 state_T)
  Common.Px po3 =
    evalPayOff optSpec t (m_St3      state_T) (m_IS3      state_T) tFin
                         (m_knockLo3 state_T) (m_knockUp3 state_T)
  sum4 = po0 + po1 + po2 + po3

  -- Also the final RNG State must be extracted from "state_T":
  rngState' :: System.Random.StdGen
  rngState' =  m_rngState   state_T

-------------------------------------------------------------------------------
-- "mcEvalBlock":                                                            --
-------------------------------------------------------------------------------
-- Evaluate a Block of Quadrupes of Monte-Carlo Paths, returns the sum of all
-- PayOffs evaluated over each individual Path:
--
mcEvalBlock :: Diffusions.Diff1D                                    ->
               Maybe (OptPricer.FwdIRModel, OptPricer.FwdDivsModel) ->
               Contracts.OptionSpec -> MCNumEnv1D  -> Int   -> Int  ->
               Common.Px            -> Common.Time -> Double

mcEvalBlock diff mbFwdCurves optSpec numEnv blockID blockSz s t  =
  runQuadrs 0 rngState0 0.0
  where
  -- Here we initialise the RNG State, but it must use different seeds for all
  -- blocks, otherwise we would get identical results across all blocks:
  rngState0 :: System.Random.StdGen
  rngState0 =  System.Random.mkStdGen (m_rngSeed numEnv + blockID)

  runQuadrs :: Int -> System.Random.StdGen -> Double -> Double
  runQuadrs    qID    rngState     currSum  =
    if qID >= blockSz
    then currSum
    else
      let
        (sum4, rngState') =
          mcEvalPaths4 diff mbFwdCurves optSpec numEnv rngState s t
      in
        runQuadrs (qID + 1) rngState' (sum4 + currSum)

  -- NB: RNG State is threaded through the block, becomes disused at the block
  -- end...

-------------------------------------------------------------------------------
-- "ckKnock": Verifies the KnockIn / KnockOut Conds at Option Barriers:      --
-------------------------------------------------------------------------------
-- Returns (CondSet, KnockOutSet):
--
ckKnock :: Contracts.OptionSpec -> Bool -> Common.Px -> Bool -> (Bool, Bool)
ckKnock    optSpec isUp s prevCond =
  if prevCond
  then
    case barr of
      Contracts.KnockOut _ -> (True,  True)   -- CondAlreadySet, IsKnockOut
      _                    -> (True,  False)  -- CondAlreadySet, NOTKnockOut
  else
    -- Similar to above, we evaluate the Cond and also return the flag whether
    -- it is actually a KnockOut:
    case barr of
      Contracts.NoBarr     -> (False, False)  -- No Conds at all, no KnockOut
      Contracts.KnockIn  b ->
        let knockIn  = (isUp && s <= b) || (not isUp && s >= b)
        in (knockIn,  False)    -- Not a KnockOut anyway
      Contracts.KnockOut b ->
        let knockOut = (isUp && s >= b) || (not isUp && s <= b)
        in (knockOut, knockOut) -- It is a KnockOut if set
  where
  barr :: Contracts.Barrier
  barr =
    (if isUp then Contracts.m_upBarrier else Contracts.m_loBarrier) optSpec

-------------------------------------------------------------------------------
-- "evalPayOff":                                                             --
-------------------------------------------------------------------------------
-- Evaluate the PayOff on the Final MC State:
--
evalPayOff ::
  --        OptSpec       t0             S(T)         Int S(t)  tFin
  Contracts.OptionSpec -> Common.Time -> Common.Px -> Double -> Common.Time ->
  -- KnockLo KnockUp      PayOff
  Bool    -> Bool      -> Common.Px

evalPayOff optSpec t sT isT tFin knockLo knockUp
  | knockedOut    = Common.Px 0
  | not knockedIn = Common.Px 0  -- If KnockIn is required
  | otherwise     = payOff
  where
  -- To be knocked out, it is sufficient to get that at EITHER barrier:
  knockedOut,     knockedOutLo,   knockedOutUp :: Bool
  knockedOut   =  knockedOutLo || knockedOutUp
  knockedOutLo =
    case Contracts.m_loBarrier optSpec of
      Contracts.KnockOut _ ->  knockLo
      _                    ->  False
  knockedOutUp =
    case Contracts.m_upBarrier optSpec of
      Contracts.KnockOut _ ->  knockUp
      _                    ->  False

  -- Whether KnockIn is required, and has actually occurred: the cond must be
  -- satisfied at BOTH barriers:
  knockedIn,      knockedInLo,   knockedInUp :: Bool
  knockedIn    =  knockedInLo && knockedInUp
  knockedInLo  =
    case Contracts.m_loBarrier optSpec of
      Contracts.KnockIn _ ->   knockLo
      _                   ->   True  -- KnockIn not required at all
  knockedInUp  =
    case Contracts.m_upBarrier optSpec of
      Contracts.KnockIn _ ->   knockUp
      _                   ->   True  -- KnockIn not required at all

  -- PayOff evaluation: Depends on the ArgType:
  poArg :: Common.Px
  poArg =
    case Contracts.m_payOffArgType optSpec of
      Contracts.FinalEuropean -> sT  -- European PayOff
      Contracts.IntegralAvg   ->     -- Asian Integral Average of S(t..T)
        let
          -- If we got here, the Option has NOT been knocked out, so the final
          -- time must be the Expir Time:
          tau :: Double
          tau =  Common.assert (tFin == tExp)
                               (Common.getTimeY tExp - Common.getTimeY t)
        in
        if   Common.assert (tau >= 0) (tau > 0)
        then Common.Px (isT / tau)
        else sT    -- Degenerate case: Single-point Path

  tExp :: Common.Time
  tExp =  Contracts.expirationTime optSpec

  -- We can finally evaluate the PayOff:
  payOff :: Common.Px
  payOff =  Contracts.evalPayOffFunc (Contracts.m_payOffFunc optSpec) poArg

