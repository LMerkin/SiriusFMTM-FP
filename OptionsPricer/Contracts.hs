-- vim:ts=2:et
-- ========================================================================= --
--                               "Contracts.hs":                             --
--      Specification of Financial Contracts: Spot, Futures, Options, etc    --
-- ========================================================================= --
module Contracts
where

import qualified Common

-------------------------------------------------------------------------------`-- Tenors (mainly for IRPs):                                                 --
-------------------------------------------------------------------------------
-- NB: M12 and Y1 are same:
--
data Tenor =
    ON | W1  | W2
  | M1 | M2  | M3 | M6 | M9 | M12
  | Y1 | M18 | Y2 | Y3 | Y5 | Y7  | Y10
  deriving(Read, Show, Eq)

-------------------------------------------------------------------------------
-- "addTenor":                                                               --
-------------------------------------------------------------------------------
-- Applying Tenors to Dates:
--
addTenor :: Common.Date -> Tenor -> Common.Date
addTenor date@(year, month, day)  tenor =
  let
    -- "adjustDateD": Date adjustment in case the DAY was incremented and is
    -- over the month boundary:
    adjustDateD :: Int -> Int -> Int -> Common.Date
    adjustDateD y m d =
      let
        dm = Common.daysInMonth y m
      in
      if d > dm
      then
        let
          m' = m + 1
          d' = d - dm
        in
          if m' > 12 then (y + 1, 1, d') else (y, m', d')
      else
        -- The date is valid as is:
        (y, m, d)

    -- "adjustDateM": Date adjustment in case the MONTH was incremented, and
    -- as a result, the day is over the month boundary; in that case, we set
    -- the day as the last day of that month:
    adjustDateM :: Int -> Int -> Int -> Common.Date
    adjustDateM y m d  =
      let dm = Common.daysInMonth y m
      in  if d > dm then (y, m, dm)  else (y, m, d)

    -- "adjustDateY": After the YEAR was incremented; this can only cause pro-
    -- blems for 29.02:
    adjustDateY :: Int -> Int -> Int -> Common.Date
    adjustDateY y m d  =
      if  m == 2 && d > Common.daysInMonth y 2
      then (y, 2, 28)
      else (y, m,  d)

    -- Apply the Tenor:
    date' =
      Common.assert (Common.isValidDate date)
      (case tenor of
        ON  -> adjustDateD  year        month      (day +  1)
        W1  -> adjustDateD  year        month      (day +  7)
        W2  -> adjustDateD  year        month      (day + 14)
        M1  -> adjustDateM  year       (month + 1)  day
        M2  -> adjustDateM  year       (month + 2)  day
        M3  -> adjustDateM  year       (month + 3)  day
        M6  -> adjustDateM  year       (month + 6)  day
        M12 -> adjustDateY (year + 1)   month       day
        Y1  -> adjustDateY (year + 1)   month       day
        M18 ->
          let (y, m, d) =  adjustDateM year (month + 6) day
          in  adjustDateY  (y+1) m d
        Y2  -> adjustDateY (year + 2)   month       day
        Y3  -> adjustDateY (year + 3)   month       day
        Y5  -> adjustDateY (year + 5)   month       day
        Y7  -> adjustDateY (year + 7)   month       day
        Y10 -> adjustDateY (year + 10)  month       day)
  in
    -- Apply the Business Day correction (always moving forward). XXX: Not imp-
    -- lemented yet, as requires a business calendar for a particulr jurisdict-
    -- ion:
    Common.assert (Common.isValidDate date') date'

-------------------------------------------------------------------------------
-- Financial Contracts:                                                      --
-------------------------------------------------------------------------------
-- NB: In all cases below, String is InstrName:
--
data Contract =
  -- Equity, FX or Commodity Contracts:
    Spot     String
  | Futures  String  Common.Time     -- ExpirTime
  | Option   String  OptionSpec
  | IRP      String  IRP
  -- Cannot derive Eq: OptionSpec.PayOffFunc may contain a lambda!
  deriving (Read, Show, Eq)

-- Specifically for Interest Rate Products:
-- XXX: No Notional is currently specified for IRS, we are interested in rates
-- only:
data IRP =
    FwdLIBOR         Common.Date Tenor  -- StartDate, Tenor
  | FwdClassicalIRS  [Common.Date]      -- (Quarterly)Floating Leg Schedule
  deriving (Read, Show, Eq)

-------------------------------------------------------------------------------
-- "PayOffFunc" for Options:                                                 --
-------------------------------------------------------------------------------
data PayOffFunc =
    Call    Common.Px                -- Strike
  | Put     Common.Px                -- Strike
  | BinCall Common.Px    Common.Px   -- Strike, Notional
  | BinPut  Common.Px    Common.Px   -- Strike, Notional
  | LinearC [ (Double,   PayOffFunc) ]
  | AnyPOF  (Common.Expr Double    ) -- Actually, this is Px, not raw Double!
  deriving (Read, Show,  Eq)

-------------------------------------------------------------------------------
-- "evalPayOffFunc":                                                         --
-------------------------------------------------------------------------------
evalPayOffFunc :: PayOffFunc -> Common.Px -> Common.Px

evalPayOffFunc (Call    (Common.Px k))   (Common.Px s) =
  Common.Px (max (s - k) 0.0)

evalPayOffFunc (Put     (Common.Px k))   (Common.Px s) =
  Common.Px (max (k - s) 0.0)

evalPayOffFunc (BinCall (Common.Px k) n) (Common.Px s) =
  if s >= k then n else  Common.Px 0

evalPayOffFunc (BinPut  (Common.Px k) n) (Common.Px s) =
  if s <= k then n else  Common.Px 0

evalPayOffFunc (LinearC lcs)  px      =
  Common.Px
    ( foldl (\currSum (coeff, poFunc) ->
          let Common.Px po = evalPayOffFunc poFunc px
          in  currSum  + coeff * po)    0.0 lcs )

evalPayOffFunc (AnyPOF f)            (Common.Px s) =
  Common.Px ((Common.mkFunc f) s)

-------------------------------------------------------------------------------
-- "PayOffArgType":                                                          --
-------------------------------------------------------------------------------
-- Specifies the PayOffFunc Argument semantics (eg the final px S_T as in
-- European options, or integral average px along the path as for standard
-- Asian    options, etc):
--
data PayOffArgType =
    FinalEuropean     -- PayOff is a function of the last S_T
  | IntegralAvg       -- PayOff is a function of (1/T int_0^T S_t dt): Asian
  -- TODO: Add other Exotic PayOffTypes 
  deriving (Read, Show, Eq)

-------------------------------------------------------------------------------
-- "ExerciseTimes" for Options:                                              --
-------------------------------------------------------------------------------
data ExerciseTimes  =
    European  Common.Time     -- Europen-type single expiration date
  | American  Common.Time     -- Can be exercised at any time <= ExpirTime
  | Bermudan [Common.Time]    -- Can be exercised at some pre-defined times
    -- TODO: ensure that Bermudan exercise times are strictly increasing
  deriving (Read, Show, Eq)

-------------------------------------------------------------------------------
-- Barrier Types for Options:                                                --
-------------------------------------------------------------------------------
data Barrier =
    NoBarr
  | KnockIn  Common.Px
  | KnockOut Common.Px
  deriving (Read, Show, Eq)

-------------------------------------------------------------------------------
-- The Over-All "OptionSpec":                                                --
-------------------------------------------------------------------------------
data OptionSpec =
  OptionSpec
  {
    m_underlying    :: Contract,     -- RECURSIVE!!!
    m_created       :: Common.Time,
    m_payOffFunc    :: PayOffFunc,
    m_payOffArgType :: PayOffArgType,
    m_exerciseTimes :: ExerciseTimes,
    m_loBarrier     :: Barrier,
    m_upBarrier     :: Barrier
  }
  deriving (Read, Show, Eq)

-------------------------------------------------------------------------------
-- "isOnFutures": Whether the Option is on Futures:                          --
-------------------------------------------------------------------------------
isOnFutures  :: OptionSpec -> Bool
isOnFutures  optSpec =
  case m_underlying optSpec of
    Contracts.Spot    _            -> False
    Contracts.Futures _ futExpTime ->
      -- Yes, the underlying is Futures, but verify the Expiration Times:
      if futExpTime < expTime
      then error ("FutExpTime="            ++ (show futExpTime) ++
                  " is before OptExpTime=" ++ (show expTime))
      else True
    _ -> error "Non-{Spot,Fut} underlying currently not supported"
  where
  expTime = expirationTime optSpec

-------------------------------------------------------------------------------
-- "expirationTime": Option Expiration Time (not always == Excersise Time):  --
-------------------------------------------------------------------------------
expirationTime :: OptionSpec -> Common.Time
expirationTime optSpec =
  case m_exerciseTimes optSpec of
  European  t -> t
  American  t -> t
  Bermudan ts ->
    case ts of
      [] -> error "Empty ExerciseTimes for a Bermudan Option"
      _  -> last ts

