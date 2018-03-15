import Data.List (maximumBy)
import Data.Function (on)

main :: IO ()
main = print answer

collatzLength :: Int -> Int
collatzLength 1 = 1
collatzLength n = 1 + collatzLength m
    where
        (q, r) = quotRem n 2
        m = if r == 0 then q else 3*n + 1

mapPair :: (a -> b) -> [a] -> [(a, b)]
mapPair f = map (\x -> (x, f x))

answer :: Int
answer = fst . maximumBy (compare `on` snd) $ mapPair collatzLength [1..1000000]
