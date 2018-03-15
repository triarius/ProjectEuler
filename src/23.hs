import           ProjectEuler.Divisors (d)
import           Data.IntSet           (IntSet)
import qualified Data.IntSet           as S

isAbundant :: Int -> Bool
isAbundant n = d n' > n'
  where n' = fromIntegral n

abundants :: [Int]
abundants = filter isAbundant [12..28111]

abundantSums :: IntSet
abundantSums = foldl (flip S.insert) S.empty
            [asum | x <- abundants,
                    y <- abundants,
                    x <= y,
                    let asum = x + y,
                    asum <= 28123]

answer :: Int
answer = 28123 * 28124 `div` 2 - S.foldr (+) 0 abundantSums

main :: IO ()
main = print answer
