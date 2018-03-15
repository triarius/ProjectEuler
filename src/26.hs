import Data.List (maximumBy)
import Data.Function (on)

main :: IO ()
main = print answer

decimals :: Int -> [(Int, Int)]
decimals = decimals' 1
    where
        decimals' :: Int -> Int -> [(Int, Int)]
        decimals' 0 _ = []
        decimals' dvnd dvsr = qr : decimals' (10 * r) dvsr
            where qr@(_, r) = dvnd `quotRem` dvsr

getCycle :: Eq a => [a] -> [a]
getCycle xs = map snd . dropTakeWhile ((==1) . flip (uncurry numPriorElem) xs) $ zip [0..] xs

dropTakeWhile :: Eq a => (a -> Bool) -> [a] -> [a]
dropTakeWhile _ []     = []
dropTakeWhile p (x:xs) = if p x then x : takeWhile p xs else dropTakeWhile p xs

numPriorElem :: Eq a => Int -> a -> [a] -> Int
numPriorElem i0 x0 xs = count x0 $ take i0 xs

count :: Eq a => a -> [a] -> Int
count x = length . filter (==x)

mapPair :: (a -> b) -> [a] -> [(a, b)]
mapPair f = map (\x -> (x, f x))

answer :: Int
answer = (fst. maximumBy (compare `on` snd) . mapPair (length . getCycle. decimals)) [2..999]
