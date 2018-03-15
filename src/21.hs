import ProjectEuler.Divisors (d)

main :: IO ()
main = print answer

isAmicable :: Integer -> Bool
isAmicable n = (d . d) n == n && d n /= n

amicablePairsTo :: Integer -> [Integer]
amicablePairsTo n = filter isAmicable [1..n]

answer :: Integer
answer = (sum . amicablePairsTo) 9999
