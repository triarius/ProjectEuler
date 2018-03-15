main :: IO ()
main = print answer

fibonacci :: [Integer]
fibonacci = 1 : 1 : zipWith (+) fibonacci (tail fibonacci)

numDigits :: Integer -> Integer
numDigits = numDigits' 0
    where
        numDigits' acc 0 = acc
        numDigits' acc n = numDigits' (acc + 1) (n `quot` 10)

answer :: Integer
answer = fst . head . dropWhile (\x -> numDigits (snd x) < 1000) $ zip [1..] fibonacci
