reversalB :: Int -> Int -> Int
reversalB b = go 0
  where go p q = if q == 0 then p else let (d, r) = quotRem q b
                                        in go (b*p + r) d

isPalindromeB :: Int -> Int -> Bool
isPalindromeB b x = reversalB b x == x

main :: IO()
main = print . sum . filter (isPalindromeB 2) . filter (isPalindromeB 10) $ [1..1000000]
