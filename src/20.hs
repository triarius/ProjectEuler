import Data.List (unfoldr)

main :: IO ()
main = print answer

factorial :: Integer -> Integer
factorial n = product [1..n]

toDigits :: Integer -> [Integer]
toDigits = unfoldr (\x -> if x == 0 then Nothing else Just $ (swap . quotRem x) 10)

swap :: (a, b) -> (b, a)
swap (x, y) = (y, x)

answer :: Integer
answer = (sum . toDigits . factorial) 100
