smallestFactor :: Integer -> Integer
smallestFactor n = head $ filter ((==0) . (n `mod`)) [2..]

factorise :: Integer -> [Integer]
factorise m = factorise' m []
  where
    factorise' :: Integer -> [Integer] -> [Integer]
    factorise' 1 factors = factors
    factorise' n factors = p : factorise' (n `div` p) factors
      where p = smallestFactor n

param :: Integer
param = 600851475143

answer :: Integer
answer = (last . factorise) param

main :: IO ()
main = print answer
