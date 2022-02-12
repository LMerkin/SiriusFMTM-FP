-- vim:ts=2:et
--
data AST a =
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
  deriving(Read, Show)

-- Symbolic differentiation of the AST (wrt X):
diff :: (Num a) => AST a -> AST a
diff X                 = Const 1
diff (Const _)         = Const 0
diff (Add  left right) = Add (diff left) (diff right)
diff (Sub  left right) = Sub (diff left) (diff right)
diff (Mult left right) = Add (Mult left  (diff right)) (Mult (diff left) right)
diff (Div  left right) =
  Div (Sub (Mult (diff left) right) (Mult (diff right) left)) (Pow right 2)
diff (Pow  e p)        = Mult (Mult (Const p) (Pow e (p-1)))  (diff e)
diff (Exp  e)          = Mult (Exp e) (diff e)
diff (Log  e)          = Div  (diff e) e
diff (Sin  e)          = Mult (Cos e) (diff e)
diff (Cos  e)          = Mult (Mult (Sin e) (diff e)) (Const (-1))

-- !!!
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

integrate  :: (Ord a, Floating a) => (a -> a) -> a -> a -> a -> a
integrate func a b step = integrateS a 0.0
  where
-- TAIL RECURSION: LOOP EQUIVALENT
  integrateS currX currSum =
    if currX >= b
    then
      currSum * step
    else
      integrateS (currX + step) (currSum + func currX)

--main :: IO ()
--main = do
  -- SEQUENTIAL:
--  exprS <- getLine
--  aS    <- getLine
--  bS    <- getLine
--  stepS <- getLine
--  let ast  = read   exprS
--  let a    = read   aS
--  let b    = read   bS
--  let step = read   stepS
--  let res  = integrate (mkFunc ast) a b step
--  putStrLn (show res)

