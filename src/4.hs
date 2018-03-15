import Data.List(unfoldr)

intToList :: Integer -> [Integer]
intToList = unfoldr (\x -> if x == 0 then Nothing else Just (x `rem` 10, x `div` 10))

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs

answer :: Integer
answer = maximum . filter (isPalindrome . intToList) $ (*) <$> [999, 998..100] <*> [999, 998..100]

main :: IO ()
main = print answer
