import Data.Set (Set)
import qualified Data.Set as S
import Data.List (foldl', unfoldr)
import Data.Tuple (swap)
import ProjectEuler.Primes (primes, isPrimeOwnList)

digits :: Integer -> [Integer]
digits = unfoldr (\x -> if x == 0 then Nothing else Just . swap . divMod x $ 10)

undigits :: [Integer] -> Integer
undigits = foldl' ((+) . (10*)) 0

rotations :: [a] -> [[a]]
rotations xs = map (take n) $ scanl (const . tail) (cycle xs) [1..n-1]
  where n = length xs

setOfRotations :: Ord a => [a] -> Set [a]
setOfRotations xs = S.fromList . map (take n) $ scanl (\acc _ -> tail acc) (cycle xs) [2..n]
  where n = length xs

isCircular :: [Integer] -> [Integer] -> Bool
isCircular ps = all (isPrimeOwnList ps . undigits) . rotations

circularBases :: [Integer] -> [Set [Integer]] -> [Set [Integer]]
circularBases ps = foldr f []
  where
    f :: Set [Integer] -> [Set [Integer]] -> [Set [Integer]]
    f xs acc = if all (isPrimeOwnList ps . undigits) xs
                  then xs : acc
                  else acc

main :: IO ()
main = do
    let ps  = takeWhile (<1000) primes
        odds = [1, 3, 7, 9] :: [Integer]
        xs1 = map pure [2, 3, 5, 7]
        xs2 = [d : ds | d <- odds, ds <- map pure odds]
        xs3 = [d : ds | d <- odds, ds <- xs2]
        xs4 = [d : ds | d <- odds, ds <- xs3]
        xs5 = [d : ds | d <- odds, ds <- xs4]
        xs6 = [d : ds | d <- odds, ds <- xs5]
        xs =  concat [xs1, xs2, xs3, xs4, xs5, xs6]
    print . length . circularBases ps . map setOfRotations $ xs
