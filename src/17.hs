main :: IO ()
main = print answer

toLetters :: Int -> String
toLetters 0 = []
toLetters 1 = "one"
toLetters 2 = "two"
toLetters 3 = "three"
toLetters 4 = "four"
toLetters 5 = "five"
toLetters 6 = "six"
toLetters 7 = "seven"
toLetters 8 = "eight"
toLetters 9 = "nine"
toLetters 10 = "ten"
toLetters 11 = "eleven"
toLetters 12 = "twelve"
toLetters 13 = "thirteen"
toLetters 14 = "fourteen"
toLetters 15 = "fifteen"
toLetters 16 = "sixteen"
toLetters 17 = "seventeen"
toLetters 18 = "eighteen"
toLetters 19 = "nineteen"
toLetters n
  | 20 <= n && n < 30 = "twenty" ++ toLetters (n - 20)
  | 30 <= n && n < 40 = "thirty" ++ toLetters (n - 30)
  | 40 <= n && n < 50 = "forty" ++ toLetters (n - 40)
  | 50 <= n && n < 60 = "fifty" ++ toLetters (n - 50)
  | 60 <= n && n < 70 = "sixty" ++ toLetters (n - 60)
  | 70 <= n && n < 80 = "seventy" ++ toLetters (n - 70)
  | 80 <= n && n < 90 = "eighty" ++ toLetters (n - 80)
  | 90 <= n && n < 100 = "ninety" ++ toLetters (n - 90)
  | 100 <= n && n < 1000 = toLetters (n `quot` 100)
        ++ "hundred"
        ++ if n `rem` 100 == 0 then "" else "and"
        ++ toLetters (n `rem` 100)
  | n == 1000 = "one" ++ "thousand"
  | otherwise = []

answer :: Int
answer = sum $ map (length . toLetters) [1..1000]
