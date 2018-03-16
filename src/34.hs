import Control.Arrow ((&&&))
import Data.IntMap (IntMap)
import qualified Data.IntMap as M
import Data.List (unfoldr)
import Data.Tuple (swap)
import ProjectEuler.DigitUpper

int2Digits :: Int -> [Int]
int2Digits = unfoldr (liftMaybePred (0==) (swap . (`quotRem` 10)))

liftMaybePred :: (a -> Bool) -> (a -> b) -> a -> Maybe b
liftMaybePred p f x = if p x then Nothing else Just . f $ x

digitFactorialSum :: Int -> Int
digitFactorialSum = foldr ((+) . factorial) 0 . int2Digits

factorial :: Int -> Int
factorial = flip (M.findWithDefault 1) mkFactorials

mkFactorials :: IntMap Int
mkFactorials = M.fromList $ foldl f [(0, 1)] [1..9]
  where
    f as@((i, a):_) x = (i+1, x*a) : as
    f _             _ = error "Should not be here"

solution :: Int
solution = sum
         . filter (uncurry (==)
         . (id &&& digitFactorialSum))
         $ [10..factorial 9 * start factorial]

main :: IO ()
main = print solution
