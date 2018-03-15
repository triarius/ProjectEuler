divisors :: Int -> [Int]
divisors n = divisors' 1
    where
        divisors' :: Int -> [Int]
        divisors' d
          | d * d > n = []
          | d * d == n = [d]
          | n `rem` d == 0 = d : n `quot` d : divisors' (d + 1)
          | otherwise = divisors' (d+1)

triangular :: Int -> Int
triangular n = n*(n+1) `quot` 2

answer :: Int
answer = fst . head $
    dropWhile (\(_, numDiv) -> numDiv <= 500) $
        mapPair (length . divisors) $
            map triangular [1..]

mapPair :: (a -> b) -> [a] -> [(a, b)]
mapPair f = map (\x -> (x, f x))

main :: IO ()
main = print answer
