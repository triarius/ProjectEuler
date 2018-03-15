answer :: Integer -> Integer
answer n = foldl lcm 1 [2..n]

main :: IO ()
main = print $ answer 20
