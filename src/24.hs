import Data.Char (intToDigit)

main :: IO ()
main = putStrLn answer

findChoices :: Int -> Int -> [Int]
findChoices card = choices' 1
    where
        choices' :: Int -> Int -> [Int]
        choices' level position
          | level == card + 1 = []
          | otherwise         = r : choices' (level + 1) q
            where (q, r) = position `quotRem` level

makePermutation :: Int -> [Int] -> [Int]
makePermutation n = reverse . fst . foldr selectNextElement ([], [0..n-1])
    where selectNextElement x (acc, set) = let (e, newSet) = extractElem x set in (e : acc, newSet)

extractElem :: Int -> [a] -> (a, [a])
extractElem _ []     = undefined
extractElem 0 (x:xs) = (x, xs)
extractElem n (x:xs) = (e, x : es)
    where (e, es) = extractElem (n-1) xs

answer :: String
answer = map intToDigit $ makePermutation 10 (findChoices 10 999999)
