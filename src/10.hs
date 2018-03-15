minus :: Ord a => [a] -> [a] -> [a]
minus xs'@(x:xs) ys'@(y:ys) = case compare x y of
                                LT -> x : minus xs ys'
                                EQ -> minus xs ys
                                GT -> minus xs' ys
minus xs _                  = xs

primesTo :: Integer -> [Integer]
primesTo m = sieve [2..m]
    where
        sieve :: [Integer] -> [Integer]
        sieve [] = []
        sieve (p:xs)
          | p*p > m   = p : xs
          | otherwise = p : sieve (xs `minus` [p*p, p*p+p..])

answer :: Integer
answer = sum $ primesTo 2000000

main :: IO ()
main = print answer
