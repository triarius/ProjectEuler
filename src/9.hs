import Prelude hiding ((^))
import qualified Prelude ((^))

(^) :: Num a => a -> Int -> a
(^) = (Prelude.^)

triples :: [[Int]]
triples = [[a, b, c] |
            a <- [0..1000],
            b <- [a + 1 .. 1000 - a],
            c <- [1000 - a - b],
            c > b,
            a^2 + b^2 == c^2]

answer :: Int
answer = (product . concat) triples

main :: IO ()
main = print answer
