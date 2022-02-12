sum1 :: [Int] -> Int
sum1 [] = 0
sum1 (x:xs) = x + sum1 xs

sum2 :: [Int] -> Int
sum2 xs = sum2int xs 0
  where
  sum2int []     currSum = currSum
  sum2int (x:xs) currSum = sum2int xs (currSum + x)

sum3 :: [Int] -> Int
sum3 xs = sum3int xs 0
  where
  sum3int []     currSum = currSum
  sum3int (x:xs) currSum = currSum' `seq` sum3int xs currSum'
    where
    currSum' = currSum + x

main :: IO ()
main = do
  putStrLn (show (sum2 [1..100000000] ))

