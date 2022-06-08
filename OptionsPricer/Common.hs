-- vim:ts=2:et
-- ========================================================================= --
--                                   "Common.hs":                            --
--      Common Types and Utils for the Haskell Options Pricing Framework     --
-- ========================================================================= --
module Common
(
  assert,
  Date,     daysInMonth, isValidDate,
  Time,     mkTimeY,     mkTimeD,        getTimeY,     getTimeD,   minTime,
  TFunc,    mkConstTF,   mkStepsTF,      mkLinSegmsTF, getConstTF,
  mapTFunc, evalTFunc,   integrateTFunc, Px(..),       Expr(..),   mkFunc
)
where
------------------------------------------------------------------------------
-- "assert":                                                                --
------------------------------------------------------------------------------
assert :: Bool -> a -> a
assert    True    a =  a
assert    False   _ =  error "Assert Failure"

-------------------------------------------------------------------------------
-- "Date" (Calendar) and "Time" as a Fractional Year in UTC:                 --
-------------------------------------------------------------------------------
type    Date   = (Int, Int, Int)                             -- (YYYY, MM, DD)
newtype Time   = Time Double deriving(Read, Show, Eq, Ord);  -- E.g. 2022.12345

-------------------------------------------------------------------------------
-- "mkTimeY" : Ctor for Tome from a Year Fraction;                           --
-- "getTimeY": Extract a Year Fraction (as Double) from Time:                --
-------------------------------------------------------------------------------
mkTimeY :: Double -> Time
mkTimeY yf
  -- Only the dates within [2000.0, 2100.0) are allowed:
  | yf < 2000.0 || yf >= 2100.0 = error ("Invalid Year: "++(show yf))
  | otherwise                   = Time yf

getTimeY :: Time -> Double
getTimeY   (Time t) = t

-------------------------------------------------------------------------------
-- Utils:                                                                    --
-------------------------------------------------------------------------------
daysInMonth :: Int -> Int -> Int
daysInMonth   year month =
  if month == 4 || month == 6   || month == 9 || month == 11
  then 30
  else
  if month  == 2
  then (if year `mod` 4 == 0 then 29 else 28)
  else 31

isValidDate :: Date -> Bool
isValidDate (year, month, day) =
  year >= 2000  && year < 2100  && month >= 1 && month <= 12 && day >= 1 &&
  day  <= daysInMonth year month

-------------------------------------------------------------------------------
-- "mkTimeD":                                                                --
-------------------------------------------------------------------------------
-- Convert the given calendar date (with day fraction) into year frction:
--
avgDaysInYear :: Double
avgDaysInYear =  365.25      -- Valid for the whole 21st century

mkTimeD :: Date -> Double -> Time
mkTimeD    date@(year, month, day) dayFrac
  -- Again, only the dates within [2000.0, 2100.0) are allowed.
  -- Checks:
  | not (isValidDate date) = error ("Invalid Date: " ++ (show date))
  | otherwise =
    -- "dayCount" is 0-based count of the day given by (month, day) in a year
    -- (01.01 = 0):
    let
      febDays  = daysInMonth year 2
      dayCount =
        case month of
          1  -> day -   1
          2  -> day +  30
          3  -> day +  30 + febDays
          4  -> day +  61 + febDays
          5  -> day +  91 + febDays
          6  -> day + 122 + febDays
          7  -> day + 152 + febDays
          8  -> day + 183 + febDays
          9  -> day + 214 + febDays
          10 -> day + 244 + febDays
          11 -> day + 275 + febDays
          12 -> day + 305 + febDays
      in
        -- Year Fraction Convention: ACT/365.25 valid for the whole 21st Cent:
        Time ((fromIntegral year) +
             ((fromIntegral dayCount + dayFrac) / avgDaysInYear))

------------------------------------------------------------------------------
-- "getTimeD": Convert "Time" into (Year, Month, Day) and DayFraction:
------------------------------------------------------------------------------
-- This is the inverse of "mkTimeD":
--
getTimeD :: Time -> (Date, Double)
getTimeD   (Time t) =
  let
    year, yearDays, month, day :: Int
    (year, fy) = assert (2000.0 <= t && t < 2100.0) (properFraction t)

    -- Fractional number of days in a year:
    fDays :: Double
    fDays = fy * avgDaysInYear

    -- Get the yearDays (0+) and conver them into Month and Day:
    (yearDays, fd) = properFraction fDays

    febDays        = daysInMonth year 2
    (month, day)   =
      if yearDays <=  30
      then  (1, yearDays + 1)
      else
      if yearDays <=  30 + febDays
      then  (2, yearDays - 30)
      else
      if yearDays <=  61 + febDays
      then  (3, yearDays - 30  - febDays)
      else
      if yearDays <=  91 + febDays
      then  (4, yearDays - 61  - febDays)
      else
      if yearDays <= 122 + febDays
      then  (5, yearDays - 91  - febDays)
      else
      if yearDays <= 152 + febDays
      then  (6, yearDays - 122 - febDays)
      else
      if yearDays <= 183 + febDays
      then  (7, yearDays - 152 - febDays)
      else
      if yearDays <= 214 + febDays
      then  (8, yearDays - 183 - febDays)
      else
      if yearDays <= 244 + febDays
      then  (9, yearDays - 214 - febDays)
      else
      if yearDays <= 275 + febDays
      then (10, yearDays - 244 - febDays)
      else
      if yearDays <= 305 + febDays
      then (11, yearDays - 275 - febDays)
      else (12, yearDays - 305 - febDays)
  in
    ((year, month, day), fd)

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
  deriving(Show, Read, Eq)

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
minTime (ConstTF _)        = error "minTime: Not defined for ConstTF"
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
-- Returns a triple:
-- (*) the value  of the integral;
-- (*) the vector of derivatives of the integral wrt all intecepts;
-- (*) the vector of derivatives of the integral wrt all slopes:
--
integrateTFunc :: TFunc ->  Time  -> Time  -> (Double, [Double], [Double])

integrateTFunc (ConstTF c) (Time a) (Time b) =
  let tau = b - a
  in  (c * tau, [tau], [0])

-- Now "f" is a "StepsT" or "SegmsTF": Ordering of limits is important:
integrateTFunc f           ta@(Time a) tb@(Time b)
  | a == b    =
    case f of
      StepsTF    steps ->
        let zeros = replicate (length steps) 0.0
        in (0.0, zeros, zeros)
      LinSegmsTF segms ->
        let zeros = replicate (length segms) 0.0
        in (0.0, zeros, zeros)

  | a >  b    =
    -- Inverted integration limits:
    let (  sum, ids,  sds) = integrateTFunc f tb ta
    in  (- sum, [- id | id <- ids], [- sd | sd <- sds])

  | otherwise =
    -- Generic case: a < b
    if a < minT
    then   error ("a=" ++ (show a) ++ ", minT=" ++ (show minT))
    else
      -- Generic Case: Integration interval [a,b] entirely belongs to the
      -- function domain:
      case f of
        StepsTF steps ->
          let
            ns = length steps
            res@(_, ids, sds) = integrTF steps integrStep 0.0 [] []
          in
            assert(length ids == ns && length sds == ns) res

        LinSegmsTF segms ->
          let
            ns = length segms
            res@(_, ids, sds) = integrTF segms integrSegm 0.0 [] []
          in
            assert(length ids == ns && length sds == ns) res
        -- ConstTF is an impossible case, hence not matched...
  where
  Time minT = minTime f

  -- NB: in "integrTF", "a" (Params) is either a Double or a pair
  -- (Double,Double):
  integrTF ::
    -- Nodes
    [(Time, a)] ->
    -- "integrNode": Node -> LeftT  -> RightT -> (SumIncr, IntcptDer, SlopeDer)
    ((Time,  a) -> Double -> Double -> (Double, Double, Double)) ->
    -- CurrSum     CurrIntcptDers CurrSlopeDers
    Double      -> [Double]    -> [Double] ->
    -- (FullIntegral, FullIntcptDers, FullSlopeDers)
    (Double, [Double], [Double])

  -- No more nodes: Return the final result:
  integrTF [] _ currSum currIDs currSDs  =
    (currSum, reverse currIDs, reverse currSDs)

   -- Reached the last node which is unbounded on the right, so the upper
   -- integration limit is always "b":
  integrTF [node@(Time t, _)]  integrNode sum ids sds =
    let
      a'   = max a t
      -- Compute the sum increment and derivatives wrt intercept  and slope
      -- over this node, modify the integral sum and the (reverse) lists of
      -- derivatives:
      (ds, iD, sD) = assert (a' <= b) (integrNode node a' b)
      sum' = sum + ds
      ids' = iD : ids
      sds' = sD : sds
    in
      -- Return the final result:
      (sum', reverse ids', reverse sds')

  integrTF (node@(Time t1, _) : tl@((Time t2, _) : _)) integrNode sum ids sds
    -- We know by construction that t1 < t2, so skip this node -- it is below
    -- the "a" integration limit:
    | a >= t2   = assert  (t1 < t2  && sum == 0.0)
                  (integrTF tl integrNode 0.0 (0.0 : ids) (0.0 : sds))
    -- a < t2:
    -- The interval end is above "a", so this interval contributes non-trivi-
    -- ally to the integral sum and its derivatives:
    | otherwise =
      let
        -- Determine the actual integration limits:
        a'   = max a t1
        b'   = min b t2
        (ds, iD, sD) = assert (a' <= b') (integrNode node a' b')
        sum' = sum + ds
        ids' = iD : ids
        sds' = sD : sds
      in
        -- Tail-recursive call:
        integrTF tl integrNode sum' ids' sds'

  -- "integrNode" is either "integrStep" or "integrSegm":
  -- "integrStep":
  integrStep :: (Time, Double) -> Double -> Double -> (Double, Double, Double)
  integrStep    (_,    val)       a'        b'      =
    let tau  = b' - a'
    in  (val * tau, tau, 0.0)

  -- "integrSegm": int_a' ^ b' (intercept + slope * (t - tFrom)) dt:
  --
  integrSegm :: (Time,  (Double, Double))  -> Double -> Double ->
                (Double, Double, Double)
  integrSegm    (Time tFrom, (intercept, slope)) a'  b' =
    let
      tau  = b' - a'
      tEff = 0.5 * (a' + b') - tFrom
    in
      (tau * (intercept + slope * tEff), tau, tau * tEff)

------------------------------------------------------------------------------
-- "Px": Price of a Tradable Instrument:                                    --
------------------------------------------------------------------------------
-- NB: We allow negative Px vals as well:
--
newtype Px = Px Double deriving (Read, Show, Eq, Ord)

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
  deriving(Read, Show, Eq)

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


