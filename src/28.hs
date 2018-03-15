import Prelude hiding ((^))
import qualified Prelude ((^))
import ProjectEuler.Divisors (isqrt)

(^) :: Num a => a -> Int -> a
(^) = (Prelude.^)

pos :: Int -> (Int, Int)
pos 1 = (0, 0)
pos n = case s of
          0 -> (radius, radius - 1 - f)
          1 -> (radius - 1 - f, -radius)
          2 -> (-radius, -radius + 1 + f)
          3 -> (-radius + 1 + f, radius)
          _ -> error "incorrect spiral"
  where
    period = roundUpEven . isqrt $ n - 1
    start  = (period - 1) ^ 2 + 1
    perim  = n - start
    radius = period `quot` 2
    (s, f) = perim `quotRem` period

roundUpEven :: Int -> Int
roundUpEven n
  | even n    = n
  | otherwise = n + 1

onDiag :: (Int, Int) -> Bool
onDiag (x, y) = abs x == abs y

answer :: Int -> Int
answer n = sum . filter (onDiag . pos) $ [1..n*n]

main :: IO ()
main = print $ answer 1001
