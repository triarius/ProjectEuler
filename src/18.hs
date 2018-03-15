main :: IO ()
main = do
    file <- readFile "triangle.txt"
    print $ findMaxPath $ map (map read . words) $ lines file

findMaxPath :: [[Int]] -> Int
findMaxPath xss = head $ foldr1 (\xs sums -> zipWith (+) (zipWith max sums $ tail sums) xs) xss
