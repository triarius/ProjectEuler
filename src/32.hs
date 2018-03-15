import qualified Data.IntSet as S
import Data.List (foldl', permutations)

pandigital :: [Int] -> [Int]
pandigital ds = pandigital' 1 4 ++ pandigital' 2 3
  where
    pandigital' :: Int -> Int -> [Int]
    pandigital' n m = [w | let w = digits2Int ws, (product . map digits2Int $ [xs, zs]) == w]
      where
        (xs, ys) = splitAt n ds
        (zs, ws) = splitAt m ys

digits2Int :: [Int] -> Int
digits2Int = foldl' ((+) . (10*)) 0

answer :: Int
answer = S.foldl' (+) 0 . S.fromList . concatMap pandigital . permutations $ [1..9]

main :: IO ()
main = print answer
