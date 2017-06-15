module Facto where

fact :: Integral a => a -> a
fact 1 = 1
fact x = x * fact (x - 1)

sumToN :: (Eq a, Num a) => a -> a
sumToN 1 = 1
sumToN n = n + sumToN (n - 1)

--fib Num a => [a] -> [a]
--fib (x:x1:xs) = ((x+x1):x:x1:xs)

-- find the nth number of the fibonacci sequence
fib :: Integral a => a -> a
fib 0 = 0
fib 1 = 1
fib n = fib(n-1) + fib(n-2)

iDiv :: Integral a => a -> a -> a
iDiv n d = case (n < d) of
  True -> 0
  False -> 1 + iDiv (n - d) d 

iDM :: Integral a => a -> a -> (a, a)
iDM n d = go n d 0
  where go n d count
         | n < d     = (count, n)
         | otherwise = go (n - d) d (count + 1)

rMul :: Integral a => a -> a -> a
rMul 1 y = y
rMul x y = y + rMul (x - 1) y

--McCarthy 91 function
-- when x > 100, out = x - 10
-- otherwise 91
mc91 :: Integral a => a -> a
mc91 n
  | n > 100  = n - 10
  | n <= 100 = mc91 (mc91 (n + 11))


--numbers into words

digitToWord :: Int -> String
digitToWord n
            | n == 0 = "zero"
            | n == 1 = "one"
            | n == 2 = "two"
            | n == 3 = "three"
            | n == 4 = "four"
            | n == 5 = "five"
            | n == 6 = "six"
            | n == 7 = "seven"
            | n == 8 = "eight"
            | n == 9 = "nine"
            | otherwise = "not a digit"

digits :: Int -> [Int]
digits n
       | n < 10 = [n]
       | otherwise = (digits $ div n 10) ++ (mod n 10 :[])

wordNumber :: Int -> String
wordNumber n = concat $ map digitToWord $ digits n




