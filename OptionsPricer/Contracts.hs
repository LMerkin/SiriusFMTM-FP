-- vim:ts=2:et
-- ========================================================================= --
--                               "Contracts.hs":                             --
--      Specification of Financial Contracts: Spot, Futures, Options, etc    --
-- ========================================================================= --
module Contracts
where

import qualified Common

-------------------------------------------------------------------------------
-- Financial Contracts:                                                      --
-------------------------------------------------------------------------------
data Contract =
    Spot    String                    -- InstrName
  | Futures String  Common.Time       -- InstrName, ExpirTime
  | Option  String  OptionSpec
  -- TODO:  Swap???
  -- Cannot derive Show or Eq: OptionSpec.PayOffFunc may contain a lambda!
  deriving (Read, Show)

-------------------------------------------------------------------------------
-- "PayOffFunc" for Options:                                                 --
-------------------------------------------------------------------------------
data PayOffFunc =
    Call    Common.Px    -- Strike
  | Put     Common.Px    -- Strike
  | LinearC [ (Double,   PayOffFunc) ]
  | AnyPOF  (Common.Expr Double    )
  deriving (Read, Show)

--
-- "evalPayOffFunc":
--
evalPayOffFunc :: PayOffFunc -> Common.Px -> Common.Px

evalPayOffFunc (Call (Common.Px k)) (Common.Px s) =
  Common.Px (max (s - k) 0.0)

evalPayOffFunc (Put  (Common.Px k)) (Common.Px s) =
  Common.Px (max (k - s) 0.0)

evalPayOffFunc (LinearC            lcs) px        =
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
  deriving (Read, Show)
 
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

