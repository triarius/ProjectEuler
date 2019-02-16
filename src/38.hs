import Math.Combinat.Permutations
import ProjectEuler.Digits (undigits)

pandigitals :: [[Integer]]
pandigitals
  = map (map toInteger. fromPermutation . (reversePermutation 9 `multiply`)) $ permutationsNaive 9

-- possible ways to split the digits
digSplits :: [[Int]]
digSplits = [[1, 2, 2, 2, 2],  [2, 2, 2, 3], [3, 3, 3], [4, 5]]

-- splits the list to a list of sub strings, sizes given by 2nd arg
splitList :: [a] -> [Int] -> [[a]]
splitList _ [] = []
splitList xs (d:ds) = let (y, ys) = splitAt d xs
                       in y : splitList ys ds

-- whether the list of is of the form [x, 2x, 3x, 4x ..]
consecMults :: [Integer] -> Bool
consecMults [] = True
consecMults (x:xs) = consecMults' x 2 xs
  where
    consecMults' _ _ [] = True
    consecMults' c n (y:ys) = (y `div` n == c) && consecMults' c (n+1) ys

isPanMult :: [Integer] -> Bool
isPanMult xs = any consecMults $ map (map (fromInteger . undigits) . splitList xs) digSplits

main :: IO ()
main = print $ undigits $ head $ filter isPanMult pandigitals
