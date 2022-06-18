-- vim:ts=2:et
--
module SymDiff (AST, diff, simplifyFP, mkFunc, test)
where

-- Symbolic representation of algebrac expressions:
data AST  a =
    X
  | Const a
  | Add  (AST a) (AST a)
  | Sub  (AST a) (AST a)
  | Mult (AST a) (AST a)
  | Div  (AST a) (AST a)
  | Pow  (AST a) a
  | Exp  (AST a)
  | Log  (AST a)
  | Sin  (AST a)
  | Cos  (AST a)
  deriving(Read, Show, Eq)

------------------------------------------------------------------------------
-- Symbolic Differentiation of the AST (wrt X):                             --
------------------------------------------------------------------------------
diff :: (Floating a) => AST a -> AST a
diff X                 = Const 1
diff (Const _)         = Const 0
diff (Add  left right) = Add (diff left) (diff right)
diff (Sub  left right) = Sub (diff left) (diff right)
diff (Mult left right) = Add (Mult left  (diff right)) (Mult (diff left)  right)
diff (Div  left right) =
  Div (Sub (diff left)  (Mult (diff right) (Div left right))) right
diff (Pow  e p)        = Mult (Mult (Const p) (Pow e (p-1))) (diff e)
diff (Exp  e)          = Mult (Exp  e) (diff e)
diff (Log  e)          = Div  (diff e) e
diff (Sin  e)          = Mult (Cos  e) (diff e)
diff (Cos  e)          = Mult (Mult (Sin e) (diff e)) (Const (-1))

------------------------------------------------------------------------------
-- "simplify" is a set of re-writing rules:                                 --
------------------------------------------------------------------------------
simplify :: (Ord a, Floating a) => (AST a) -> (AST a)

-- X, Const:
simplify X                          = X
simplify (Const a)                  = Const a

-- Add:
simplify (Add  (Const l) (Const r)) = Const (l + r)
simplify (Add  (Const l) right)     = Add (simplify right) (Const l)
simplify (Add  left  (Const 0))     = simplify left
simplify (Add  l@(Mult (Const a) left) r@(Mult (Const b) right))
  | left == right = Mult (Const (a + b)) (simplify left)
  | otherwise     = Add  (simplify l)    (simplify r)

-- Add catch-all:
simplify (Add  left  right)         =
  if left' == right'
  then Mult (Const 2) left'
  else Add  left'     right'
  where
  left'  = simplify left
  right' = simplify right

-- Sub:
simplify (Sub  (Const l) (Const r)) = Const (l - r)
simplify (Sub  left  (Const 0))     = simplify left
simplify (Sub  (Const 0) right)     = Mult  (Const (-1)) (simplify right)
simplify (Sub  left  right)         =
  if left' == right'
  then Const 0
  else Sub left' right'
  where 
  left'  = simplify left
  right' = simplify right

-- Mult:
simplify (Mult (Const l) (Const r)) = Const (l * r)
simplify (Mult left cr@(Const r))   = Mult cr (simplify left)
simplify (Mult (Const 0) _    )     = Const 0
simplify (Mult (Const 1) right)     = simplify right
-- Associativity:
simplify (Mult (Const a) (Mult (Const b) right)) =
  Mult (Const (a * b)) (simplify right)
-- Mult catch-all:
simplify (Mult left  right)         = Mult (simplify left) (simplify right)

-- Div:
simplify (Div _    (Const 0))       = error "simplify: Denom is 0"
simplify (Div (Const l) (Const r))  = Const (l / r)
simplify (Div left (Const 1))       = simplify left
simplify (Div left m1@(Const (-1))) = Mult  m1 (simplify left)
simplify (Div (Const 0) _)          = Const 0
simplify (Div left right)           = Div (simplify left) (simplify right)

-- Pow:
simplify (Pow (Const l) p)         = Const (l ** p)
simplify (Pow left      0)         = Const 1
simplify (Pow left      1)         = simplify left
simplify (Pow left      p)         =
  -- Perform recursive simplification if "left" has been simplified:
  if left' /= left then simplify pow' else pow'
  where
  left' = simplify left
  pow'  = Pow left' p

-- Exp:
simplify (Exp (Const a)) = Const (exp a)
simplify (Exp e)         = Exp   (simplify e)

-- Log:
simplify (Log (Const a)) = Const (log a)
simplify (Log e)         = Log   (simplify e)

-- Sin:
simplify (Sin (Const a)) = Const (sin a)
simplify (Sin e)         = Sin   (simplify e)

-- Cos:
simplify (Cos (Const a)) = Const (cos a)
simplify (Cos e)         = Cos   (simplify e)

-- Simplify an expression until a Fixed Point is reached:
simplifyFP :: (Ord a, Floating a) => (AST a) -> (AST a)
simplifyFP ast =
  if   ast' == ast
  then ast   -- Reached the fixed point
  else simplifyFP ast'
  where
  ast' = simplify ast

-------------------------------------------------------------------------------
-- "mkFunc": Generating an executable lambda from an AST:                    --
-------------------------------------------------------------------------------
mkFunc :: (Floating a) => AST a -> (a -> a)
mkFunc X             = \x -> x
mkFunc (Const c)     = \_ -> c
mkFunc (Add   e1 e2) = \x -> (mkFunc e1) x + (mkFunc e2) x
mkFunc (Sub   e1 e2) = \x -> (mkFunc e1) x - (mkFunc e2) x
mkFunc (Mult  e1 e2) = \x -> (mkFunc e1) x * (mkFunc e2) x
mkFunc (Div   e1 e2) = \x -> (mkFunc e2) x / (mkFunc e2) x
mkFunc (Pow   e  p)  = \x -> ((mkFunc e) x)  ** p
mkFunc (Exp   e)     = \x -> exp ((mkFunc e) x)
mkFunc (Log   e)     = \x -> log ((mkFunc e) x)
mkFunc (Sin   e)     = \x -> sin ((mkFunc e) x)
mkFunc (Cos   e)     = \x -> cos ((mkFunc e) x)

--main :: IO ()
--main = do
  -- SEQUENTIAL:
--  exprS <- getLine
--  let ast  = read   exprS
--  putStrLn (show res)
--

test :: Double -> (Double, Double)
test x = (y x, y' x)
  where
  ast :: AST Double
  ast = Mult (Const 5) (Cos (Mult X X))

  der :: AST Double
  der = simplifyFP (diff ast)

  y   :: Double -> Double
  y   = mkFunc ast

  y'  :: Double -> Double
  y'  = mkFunc der

-- f = Mult (Const 5) (Cos (Mult X X))
-- d1 = simplifyFP (diff f)
-- show d1
-- "Mult (Const (-5.0)) (Mult (Sin (Mult X X)) (Mult (Const 2.0) X))"
--
-- d2 = simplifyFP (diff d1)
-- show d2
-- "Mult (Const (-5.0)) (Add (Mult (Const 2.0) (Sin (Mult X X))) (Mult (Mult (Cos (Mult X X)) (Mult (Const 2.0) X)) (Mult (Const 2.0) X)))"


