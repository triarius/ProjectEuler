import Data.List (sort)

main :: IO ()
main = do
    file <- readFile  "p022_names.txt"
    print $ calcScore file

score :: Char -> Int
score c = fromEnum c - fromEnum 'A' + 1

nameList :: String -> [String]
nameList x = fst . head $ readList ('[' : x ++ "]")

calcScore :: String -> Int
calcScore text = sum $ zipWith (*) scores [1..]
    where scores = map (foldl (\acc x -> score x + acc) 0) $ sort $ nameList text
