map1 :: (a -> b) -> [a] -> [b]
map1 _ []     = []
map1 f (x:xs) = (f x) : (map1 f xs)
-- Non-Tail-Recursive

reverse1 :: [a] -> [a]
reverse1 xs = reverse1' xs []
  where
  reverse1' []     res  = res
  reverse1' (x:xs) res' = reverse1' xs (x : res')

map2 :: (a -> b) -> [a] -> [b]
map2 f xs = map2' f xs []
  where
  map2' _ []     res  = reverse1 res
  map2' f (x:xs) res' = map2' f xs ((f x) : res')

-- Appending an element to the END of a list:
append :: [a] -> a -> [a]
append xs x = append' xs []
  where
  append' []     res  = reverse1    (x : res)
  append' (x:xs) res' = append'  xs (x : res')

append2 :: [a] -> a -> [a]
append2 xs x = reverse1 (x : (reverse1 xs))

-- Concatentation of lists:
concat1 :: [a] -> [a] -> [a]
concat1    xs     ys  =  concat' (reverse1 xs) ys
  where
  concat' []     res  = res
  concat' (x:xs) res' = concat' xs (x : res')

-- Left fold:
-- "b": accumulator type
foldl2  :: (b -> a -> b) -> b -> [a] -> b
foldl2  f acc0 xs = foldl2' acc0 xs
  where
  foldl2' acc  []     = acc
  foldl2' acc' (x:xs) = foldl2' (f acc' x) xs

-- BETTER:
foldl3  :: (b -> a -> b) -> b -> [a] -> b
foldl3  _ acc []     = acc
foldl3  f acc (x:xs) = foldl3 f (f acc x) xs

length1 :: [a] -> Int
length1 xs = foldl3 (\currLen _ -> currLen + 1) 0 xs

-- Average length of a string in a list of strings:
avgStrLen :: [String] -> Double
avgStrLen [] = 0.0
avgStrLen xs = (fromIntegral (sumStrLen xs)) / (fromIntegral (length1 xs))
  where
--sumStrLen xs = foldl3 (\currSum currStr -> currSum + (length1 currStr)) 0 xs
--As partial application of "foldl3":
  sumStrLen    = foldl3 (\currSum currStr -> currSum + (length1 currStr)) 0

-- BAD:
fact1 :: Int -> Integer
fact1 n
  | n <= 0    = 1
  | otherwise = (toInteger n) * (fact2 (n-1))

fact2 ::  Int -> Integer
fact2 n = (toInteger n) * (fact1 (n-1))

-- Right fold:
foldr2 :: (a -> b -> b) -> b -> [a] -> b
foldr2  f acc0 xs = foldr2' xs
  where
  foldr2' []     = acc0
  foldr2' (x:xs) = f x (foldr2' xs)

foldr3 :: (a -> b -> b) -> b -> [a] -> b
foldr3    _ acc []      = acc
foldr3    f acc' (x:xs) = f x (foldr3 f acc' xs)

-- CURRYING AND PARTIAL APPLICATION:
f1 :: Num a => a -> a
f1 x = x + 1

y1 :: Int
y1 = f1 (10   :: Int)
y2 :: Double
y2 = f1 (10.1 :: Double)

f2 :: Num a => a -> a
f2 = \x -> x + 1

y4 :: Double
y4 = f2 (10.1 :: Double)
y3 :: Int
y3 = f2 (10   :: Int)

-- Curried function:
f3 :: Num a => a -> a -> a
f3 x y = x + y

-- Partial application: 1+y
-- Setting x=1:
f31 :: Num a => a -> a
f31 = f3 1

f32 :: Num a => a -> a
f32 = \x -> f3 x 5

-- Uncurried function: BAD IDEA:
f4 :: Num a => (a, a) -> a
f4 (x, y) = x + y

f41 :: Num a => a -> a
f41 = \y -> f4 (1, y)

f42 :: Num a => a -> a
f42 = \x -> f4 (x, 5)

main :: IO ()
main = do
  putStrLn (show(f4 (1, 2)))

