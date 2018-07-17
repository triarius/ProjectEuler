import Data.List (unfoldr)
import ProjectEuler.Primes (primes, isPrime)

ltruncates :: Integer -> [Integer]
ltruncates = unfoldr go
  where go x
          | x < 10 = Nothing
          | otherwise = let q = x `quot` 10
                         in Just (q, q)

rtruncates :: Integer -> [Integer]
rtruncates x = unfoldr go 10
  where go p
          | p > x = Nothing
          | otherwise = let r = x `rem` p
                         in Just (r, 10*p)

truncateable :: Integer -> Bool
truncateable x = all isPrime $ ltruncates x ++ rtruncates x

main :: IO ()
main = print . sum . take 11 . filter truncateable . drop 4 $ primes
