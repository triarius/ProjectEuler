module ProjectEuler.Divisors where

(^!) :: Num a => a -> Integer -> a
(^!) x n = x ^ n

isqrt :: (Integral a) => a -> a
isqrt 0 = 0
isqrt 1 = 1
isqrt n = (head . dropWhile isNotRoot . iterate newtonRapson) x0
    where
        isNotRoot x = x^!2 > n
        newtonRapson x = (x^!2 + n) `div` (2*x)
        x0 = n `div` 2

factors :: Integer -> [Integer]
factors 0 = []
factors 1 = []
factors n = 1 : factors' [2..isqrtn - 1]
  where
    isqrtn = isqrt n
    factors' []
      | isqrtn^!2 == n = [isqrtn]
      | r == 0 = [isqrtn, q]
      | otherwise = []
      where (q, r) = n `quotRem` isqrtn
    factors' (x:xs) = if r == 0 then x : q : factors' xs else factors' xs
      where (q, r) = n `quotRem` x

d :: Integer -> Integer
d = sum . factors
