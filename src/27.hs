import ProjectEuler.Primes (isPrime, primes)
import Data.List (maximumBy, genericLength)
import Data.Function (on)
import Control.Arrow ((&&&))

coeffcients :: [[Integer]]
coeffcients = [[1, a, b] | a <- [-999..999], b <- primesModLessThan1000]
    where primesLessThan1000 = takeWhile (<1000) primes
          primesModLessThan1000 = map negate primesLessThan1000 ++ primesLessThan1000

eval :: [Integer] -> Integer -> Integer
eval q x = foldl (\acc a -> acc*x + a) 0 q

numPrimeValues :: [Integer] -> Integer
numPrimeValues q = genericLength . takeWhile isPrime $ map (eval q) [0..]

answer :: Integer
answer = snd . maximumBy (compare `on` fst) . map (numPrimeValues &&& product) $ coeffcients

main :: IO ()
main = print answer
