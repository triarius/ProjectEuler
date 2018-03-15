fibs :: [Integer]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

answer :: Integer
answer = sum $ filter even $ takeWhile (<=4000000) fibs

main :: IO ()
main = print answer
