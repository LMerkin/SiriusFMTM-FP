-- vim:ts=2:et
-- ========================================================================= --
--                                   "Common.hs":                            --
--      Common Types and Utils for the Haskell Options Pricing Framework     --
-- ========================================================================= --
module Common
(
  assert,
  Time,     mkTimeY,   m_getTimeY,
  TFunc,    mkConstTF, mkStepsTF,      mkLinSegmsTF, getConstTF,
  mapTFunc, evalTFunc, integrateTFunc, Px(..),       Expr(..),   mkFunc
)
where
------------------------------------------------------------------------------
-- "assert":                                                                --
------------------------------------------------------------------------------
assert :: Bool -> a -> a
assert    True    a =  a
assert    False   _ =  error "Assert Failure"

------------------------------------------------------------------------------
-- "Time" as a Fractional Year in UTC:                                      --
------------------------------------------------------------------------------
newtype Time = Time { m_getTimeY :: Double} -- E.g. 2022.12345
  deriving(Read, Show, Eq, Ord);

mkTimeY :: Double -> Time
mkTimeY yf
  | yf < 2000.0 || yf >= 2100.0 = error ("Invalid Year: "++(show yf))
  | otherwise                   = Time yf

-- TODO: WallClock -> Time conversion
-- Year Fraction Convention: ACT/365.25 valid for the whole 20th and 21st Cents

------------------------------------------------------------------------------
-- "TFunc":                                                                 --
------------------------------------------------------------------------------
-- Functions of Time: either a Const, or a Step-Wise function:
--
data TFunc =
    ConstTF    Double
  | StepsTF    [(Time,  Double)]          -- [(tFrom,  val   )]
  | LinSegmsTF [(Time, (Double, Double))] -- [(tFrom, (intercept, slope))]
                                          --   intercept + slope * (t - tFrom)
  deriving(Show)

--
-- "mkConstTF":
-- Constant to TFunc:
--
mkConstTF :: Double -> TFunc
mkConstTF =  ConstTF

--
-- "getConstTF":
--
getConstTF :: TFunc -> Double
getConstTF (ConstTF c) = c
getConstTF _           = error "getConstTF: Not a ConstTF"

--
-- "mkStepsTF":
-- A list (of step-wise times and vals) to TFunc:
--
mkStepsTF :: [(Time, Double)] -> TFunc
mkStepsTF    []  = error "mkStepsTF: Empty steps"
mkStepsTF steps@(_:_)
  | verifyTNodes (map fst steps)     = StepsTF steps
  | otherwise    = error "mkStepsTF: Non-increasing Times in steps"

--
-- "mkLinSegmsTF":
--
mkLinSegmsTF :: [(Time, (Double, Double))] -> TFunc
mkLinSegmsTF [] = error "mkLinSegmsTF: Empty segms"
mkLinSegmsTF segms@(_:_)
  | verifyTNodes (map fst segms)     = LinSegmsTF segms
  | otherwise   = error "mkLinSegmsTF: Non-increasing Times in segms"

--
-- "verifyTNodes":
-- Helper function which verifes that the given times are strictly increasing:
--
verifyTNodes :: [Time] -> Bool
verifyTNodes    []   = True      -- Not really matched, just for completeness
verifyTNodes    [_]  = True
verifyTNodes    ((Time t1) : tl@((Time t2) : _)) =
  if t1 < t2
  then  verifyTNodes tl
  else  False

--
-- "mapTFunc":
--
mapTFunc :: (Double -> Double) -> TFunc -> TFunc
mapTFunc f  (ConstTF    c)     =  ConstTF (f c)
mapTFunc f  (StepsTF    steps) =  StepsTF [(t, f val) | (t, val) <- steps]
-- XXX: Unfortunately we cannot apply an arbitrary map to linear segments:
mapTFunc _  (LinSegmsTF _)     =  error "mapTFunc: Unsupported for LinSegmsTF"

--
-- "minTime":
-- Returns the minimum Time for which this TFunc is defined:
-- We assume that the domain of this TFunc is non-empty (as guaranteed by the
-- "mk..." factories above:
--
minTime :: TFunc -> Time
minTime (ConstTF _)        = Time (- (read "Infinity")::Double)
-- NB: When "TFunc" is constructed explicitly, the minimum time is 2000.0, not
-- minus infinity!
minTime (StepsTF    steps) = fst  (head steps)
minTime (LinSegmsTF segms) = fst  (head segms)

------------------------------------------------------------------------------
-- "evalTFunc":                                                             --
------------------------------------------------------------------------------
-- "TFunc" evaluated into Double at a specified instance of Time:
--
evalTFunc :: TFunc        -> Time -> Double
evalTFunc (ConstTF    c)     _    =  c

evalTFunc (StepsTF    steps) time =  snd (findApplNode steps time)

evalTFunc (LinSegmsTF segms) time@(Time t)   =
  let (Time ti, (a, b)) = findApplNode segms time
  in  assert  (t >= ti)   (a + b * (t - ti))

--
-- "findApplNode":
-- Helper function: Find the latest tFrom <= t, ie the applicable interval and
-- its value:
--
findApplNode :: [(Time, a)] -> Time    -> (Time, a)
findApplNode    nodes         (Time t) =
  findApplNode' nodes errNode
  where
  findApplNode' :: [(Time,a)] ->                 (Time, a) -> (Time, a)
  findApplNode' []                               prevNode  = prevNode
  findApplNode' (node@(Time tFrom, _) : nodes')  prevNode  =
    if   tFrom > t
    then prevNode
    else
      if   tFrom == t
      then node
      else findApplNode' nodes' node

  -- Min time in the "nodes", as a String:
  minTS :: String
  minTS =
    case nodes of
      ((Time t', _):_) -> show t'
      _                -> "None"

  -- There is no "prevNode" before the 1st list element, so use "errNode":
  errNode :: (Time, a)
  errNode =  error ("t=" ++ (show t) ++ " must be >= " ++ minTS)

-------------------------------------------------------------------------------
-- "integrateTFunc":                                                         --
-------------------------------------------------------------------------------
integrateTFunc :: TFunc -> Time  -> Time  -> Double

integrateTFunc (ConstTF c) (Time a) (Time b) = c * (b - a)

-- Now "f" is a "StepsT" or "SegmsTF": Ordering of limits is important:
integrateTFunc f           ta@(Time a) tb@(Time b)
  | a == b    = 0
  | a >  b    = - (integrateTFunc f tb ta)
  | otherwise =
      -- Generic case: a < b
      if a < minT
      then   error ("a=" ++ (show a) ++ ", minT=" ++ (show minT))
      else
        -- Generic Case: Integration interval [a,b] entirely belongs to the
        -- function domain:
        case f of
          StepsTF    steps -> integrTF steps integrStep 0.0
          LinSegmsTF segms -> integrTF segms integrSegm 0.0
          -- ConstTF is an impossible case, hence not matched
  where
  Time minT = minTime f

  -- NB: in "integrTF", "a" (Params) is either a Double or a pair
  -- (Double,Double):
  integrTF ::
    -- Nodes       (Node          LeftT     RightT     LocInt)    CurrSum
    [(Time, a)] -> ((Time,  a) -> Double -> Double  -> Double) -> Double  ->
    -- FullIntegral
    Double

  integrTF    []                  _                currSum =  currSum
  integrTF    [node@(Time t, _)]  integrNode       currSum =
    -- Reached the last node which is unbounded on the right, so the upper
    -- integration limit is always "b":
    let a' = max a t
    in  assert (a' <= b)   (currSum + integrNode node a' b)

  integrTF    (node@(Time t1, _) : tl@((Time t2, _) : _)) integrNode currSum
    -- We know by construction that t1 < t2
    | a >= t2   = assert  (t1 < t2  && currSum == 0.0)
                          (integrTF tl integrNode 0.0)
    -- a < t2:
    | otherwise =
      let
        a' = max a t1
        b' = min b t2
      in
        assert (a' <= b')
               (integrTF tl integrNode (currSum + integrNode node a' b'))

  -- "integrStep":
  integrStep :: (Time, Double) -> Double -> Double -> Double
  integrStep    (_,    val)       a'        b'      = val * (b' - a')

  -- "integrSegm": int_a' ^ b' (intercept + slope * (t - tFrom)) dt:
  --
  integrSegm :: (Time, (Double,Double))  -> Double -> Double -> Double
  integrSegm    (Time tFrom, (intercept, slope)) a'   b'     =
    (b' - a') * (intercept + slope * (0.5 * (a' + b') - tFrom))

------------------------------------------------------------------------------
-- "Px": Price of a Tradable Instrument:                                    --
------------------------------------------------------------------------------
-- NB: We allow negative Px vals as well:
--
newtype Px = Px {m_getPxVal :: Double} deriving (Read, Show, Eq, Ord)

-------------------------------------------------------------------------------
-- Symbolic Exprs:                                                           --
-------------------------------------------------------------------------------
data Expr a =
    X
  | Const a
  | Add  (Expr a) (Expr a)
  | Sub  (Expr a) (Expr a)
  | Mult (Expr a) (Expr a)
  | Div  (Expr a) (Expr a)
  | Pow  (Expr a) a
  | Exp  (Expr a)
  | Log  (Expr a)
  | Sin  (Expr a)
  | Cos  (Expr a)
  | Min  (Expr a) (Expr a)
  | Max  (Expr a) (Expr a)
  deriving(Read, Show)

------------------------------------------------------------------------------
-- "mkFunc": Generating an executable lambda from an Expr:                  --
------------------------------------------------------------------------------
mkFunc :: (Ord a, Floating a) => Expr a -> (a -> a)
mkFunc X             = \x -> x
mkFunc (Const c)     = \_ -> c
mkFunc (Add   e1 e2) = \x -> (mkFunc e1) x + (mkFunc e2) x
mkFunc (Sub   e1 e2) = \x -> (mkFunc e1) x - (mkFunc e2) x
mkFunc (Mult  e1 e2) = \x -> (mkFunc e1) x * (mkFunc e2) x
mkFunc (Div   e1 e2) = \x -> (mkFunc e2) x / (mkFunc e2) x
mkFunc (Pow   e  p)  = \x -> ((mkFunc e) x)  ** p
mkFunc (Exp   e)     = \x -> exp ((mkFunc e)  x)
mkFunc (Log   e)     = \x -> log ((mkFunc e)  x)
mkFunc (Sin   e)     = \x -> sin ((mkFunc e)  x)
mkFunc (Cos   e)     = \x -> cos ((mkFunc e)  x)
mkFunc (Min   e1 e2) = \x -> min ((mkFunc e1) x) ((mkFunc e2) x)
mkFunc (Max   e1 e2) = \x -> max ((mkFunc e1) x) ((mkFunc e2) x)


