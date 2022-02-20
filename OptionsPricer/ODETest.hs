module ODETest
where
import TimeSteppers

f :: [Double] -> Double -> [Double]
f    [x0, x1] _  =         [x0, -x1]
f    _ _         = error "f: Dim=2 is required"

xf0 = [1.0, 1.0]
tf0 = 0
tf1 = 10

g :: [Double] -> Double -> [Double]
g    [x]      _  = [1.0 + x * x ]
g    _ _         = error "g: Dim=1 is required"

xg0 = [0.0]
tg0 = 0.0
tg1 = 1.4

eps = 1e-9

runf tau = integrateODEsL f xf0 tf0 tf1 RKF5 tau eps

rung tau = integrateODEsL g xg0 tg0 tg1 RKF5 tau eps

