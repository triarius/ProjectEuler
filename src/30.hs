import Control.Arrow ((&&&))
import Data.List (unfoldr)
import Data.Tuple (swap)
import ProjectEuler.DigitUpper

digitPowerSum :: Int -> Integer -> Integer
digitPowerSum p = sum . map (^p) . digits

digits :: Integer -> [Integer]
digits = unfoldr (\x -> if x == 0 then Nothing else Just (swap (x `divMod` 10)))

selfDigitPowerSum :: Int -> Int -> [Integer]
selfDigitPowerSum p d = filter (uncurry (==) . (id &&& digitPowerSum p)) [10..10^d - 1]

solution :: Int -> Integer
solution n = sum . selfDigitPowerSum n . start $ (^n)

main :: IO ()
main = print $ solution 5
