import Prelude hiding ((^))
import qualified Prelude ((^))

(^) :: Num a => a -> Int -> a
(^) = (Prelude.^)

answer :: Integer -> Integer
answer n = (3*n^4 + 2*n^3-3*n^2-2*n) `div` 12

main :: IO ()
main = print $ answer 100
