import Data.List (unfoldr)
import Prelude hiding ((^))
import qualified Prelude ((^))

(^) :: Num a => a -> Int -> a
(^) = (Prelude.^)

main :: IO ()
main = print answer

digits :: Integer -> [Integer]
digits = unfoldr (\x -> if x == 0 then Nothing else Just $ (swap . quotRem x) 10)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

answer :: Integer
answer = (sum . digits) (2^1000)
