import Data.Ratio

fractions :: [(Int, Int, Int, Int)]
fractions = [(n1, n2, d1, d2) | n1 <- [1..9],
                                n2 <- [1..9],
                                d1 <- [n1..9],
                                d2 <- [1..9],
                                n1 < d1 || n1 == d1 && n2 < d2]

isCurious :: (Int, Int, Int, Int) -> Bool
isCurious (n1, n2, d1, d2) = (n2 == d1 && n * d2 == d * n1) || (n1 == d2 && n * d1 == d * n2)
  where n = 10*n1 + n2
        d = 10*d1 + d2

mkFrac :: (Int, Int, Int, Int) -> Ratio Int
mkFrac (n1, n2, d1, d2) = (10*n1 + n2) % (10*d1 + d2)

main :: IO ()
main = print . denominator . product. map mkFrac . filter isCurious $ fractions
