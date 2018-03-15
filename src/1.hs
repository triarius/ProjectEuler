answer :: Int
answer = sum . filter (\x -> x `mod` 5 == 0 || x `mod` 3 == 0) $ [1..1000]

main :: IO ()
main = print answer
