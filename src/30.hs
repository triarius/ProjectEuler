import Control.Arrow ((&&&))
import Data.List (unfoldr)
import Data.Tuple (swap)
import Math.NumberTheory.Logarithms

nfix :: (Int -> Int) -> Int
nfix f = fst . head . dropWhile (uncurry (/=)) . map (id &&& f) $ [1..]

-- the upper bound for the searh may be determined by solving
-- d = nDigits(d * 9^p) for positive integers d and a fixed p
start :: Int -> Int
start p = nfix (nDigits . ((9 :: Integer)^p *) . fromIntegral)

nDigits :: Integer -> Int
nDigits n = integerLog10 n + 1

digitPowerSum :: Int -> Integer -> Integer
digitPowerSum p = sum . map (^p) . digits

digits :: Integer -> [Integer]
digits = unfoldr (\x -> if x == 0 then Nothing else Just (swap (x `divMod` 10)))

solution :: Int -> Int -> Integer
solution p d = sum . filter (uncurry (==) . (id &&& digitPowerSum p)) $ [10..10^d - 1]

main :: IO ()
main = print $ solution 5 (start 5)
