module ProjectEuler.DigitUpper (start) where

import Control.Arrow ((&&&))
import Math.NumberTheory.Logarithms

nDigits :: Integer -> Int
nDigits n = integerLog10 n + 1

nfix :: (Int -> Int) -> Int
nfix f = fst . head . dropWhile (uncurry (/=)) . map (id &&& f) $ [1..]

-- the upper bound for the search may be determined by solving
-- d = nDigits(d * 9^p) for positive integers d and a fixed p
start :: Integral a => (a -> a) -> Int
start f = nfix (nDigits . toInteger . (f 9 *) . fromIntegral)
