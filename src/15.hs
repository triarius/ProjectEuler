main :: IO ()
main = print $ numPaths 20 20

numPaths :: Integer -> Integer -> Integer
numPaths n m = product [(n+1)..(n + m)] `quot` product [1..m]
