module ProjectEuler.Digits (digits, undigits) where

import Data.List (unfoldr, foldl')
import Data.Tuple (swap)

digits :: Integer -> [Integer]
digits = unfoldr (\x -> if x == 0 then Nothing else Just . swap . divMod x $ 10)

undigits :: [Integer] -> Integer
undigits = foldl' ((+) . (10*)) 0
