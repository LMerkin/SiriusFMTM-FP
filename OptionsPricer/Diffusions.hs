module Diffusions
(
  VolType1D(..), Diff1D, getVolType1D, getMu1D, getSigma1D, isNonNeg,
  mkGBM
)
where
import qualified Common

------------------------------------------------------------------------------
-- 1-Dimensional Diffusions:                                                --
------------------------------------------------------------------------------
-- dS = mu(S,t;p) dt + sigma(S,t;p) dW
--
-- For various purposes, Vols are presented in both Symbolic and Lambda forms:
--
data VolType1D =
  -- Brownian Motion:           sigma(t) * dW:     TFunc is "sigma":
    BM       Common.TFunc
  -- Geometric Brownian Motion: sigma(t) * S * dW; TFunc is "sigma":
  | GBM      Common.TFunc
  -- Constant Elasticity of Variance: (sigma(t) * S^beta) * dW: similar:
  | CEV      Common.TFunc Double

  -- Lipton's Hyperbolic Vol:   (sigma0(t) + sigma1(t)*S + sigma2(t)*S^2) * dW
  | LHV      Common.TFunc Common.TFunc Common.TFunc

  -- Any other: arbitrary function of (S,t) * dW:
  | AnyVol1D (Common.Px -> Common.Time -> Double)

-- TODO: For the moment, we do not provide a symbolic form for the trend "mu"...

data Diff1D = Diff1D
  {
    m_volType  :: VolType1D,
    m_mu       :: Common.Px -> Common.Time -> Double,
    m_sigma    :: Common.Px -> Common.Time -> Double,
    m_isNonNeg :: Bool
  }

-------------------------------------------------------------------------------
-- Accessors for the "Diff1D" flds (since the ctors are not exported):       --
-------------------------------------------------------------------------------
getVolType1D   :: Diff1D -> VolType1D
getVolType1D   =  m_volType

getMu1D        :: Diff1D -> (Common.Px -> Common.Time -> Double)
getMu1D        =  m_mu

getSigma1D     :: Diff1D -> (Common.Px -> Common.Time -> Double)
getSigma1D     =  m_sigma

isNonNeg       :: Diff1D -> Bool
isNonNeg       =  m_isNonNeg

------------------------------------------------------------------------------
-- GBM:                                                                     --
------------------------------------------------------------------------------
-- mu   (S,t) = muC       * S
-- mu   (S,t) = muT(t)    * S
--
-- sigma(S,t) = sigmaC    * S
-- sigma(S,t) = sigmaT(t) * S

mkGBM :: Common.TFunc -> Common.TFunc -> Diff1D
mkGBM    muTF     sigmaTF  =
  Diff1D
  {
    m_volType  = (GBM sigmaTF),
    m_mu       = \(Common.Px s) t -> (Common.evalTFunc muTF    t) * s,
    m_sigma    = \(Common.Px s) t -> (Common.evalTFunc sigmaTF t) * s,
    m_isNonNeg = True
  }

