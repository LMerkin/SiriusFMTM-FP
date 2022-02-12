module Math.GaussianQuadratureIntegration (nIntegrate128, nIntegrate256, nIntegrate512, nIntegrate1024) where

import Math.GaussianQuadratureRules
import Data.List

baseCase :: (Fractional a) => (a -> a) -> [a] -> [a] -> a
baseCase func points weights = foldl' (+) 0 $ map (\(x,w) -> w*(func x + func(-x))) $ zip points weights

nIntegrate128 :: (Fractional a) => (a -> a) -> a -> a -> a
nIntegrate128 func a b = 0.5*(b-a) * (baseCase (\x -> func $ 0.5*((b-a)*x+b+a)) points128 weights128)

nIntegrate256 :: (Fractional a) => (a -> a) -> a -> a -> a
nIntegrate256 func a b = 0.5*(b-a) * (baseCase (\x -> func $ 0.5*((b-a)*x+b+a)) points256 weights256)

nIntegrate512 :: (Fractional a) => (a -> a) -> a -> a -> a
nIntegrate512 func a b = 0.5*(b-a) * (baseCase (\x -> func $ 0.5*((b-a)*x+b+a)) points512 weights512)

nIntegrate1024 :: (Fractional a) => (a -> a) -> a -> a -> a
nIntegrate1024 func a b = 0.5*(b-a) * (baseCase (\x -> func $ 0.5*((b-a)*x+b+a)) points1024 weights1024)
