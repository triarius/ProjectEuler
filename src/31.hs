coins :: [Int]
coins = reverse [1, 2, 5, 10, 20, 50, 100, 200]

ways :: [Int] -> Int -> Int
ways [] 0 = 1
ways [] _ = 0
ways (c:cs) total = case compare total 0 of
                      GT -> ways cs total + ways (c:cs) (total - c)
                      EQ -> 1
                      LT -> 0

main :: IO ()
main = print $ ways coins 200
