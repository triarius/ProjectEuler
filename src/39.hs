import Data.List (maximumBy)
import Data.Function (on)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IntSet
import Data.Set (Set)
import qualified Data.Set as Set

triples :: Int -> Set IntSet
triples x = Set.fromList [ IntSet.fromList [k * (m*m - n*n), 2 * k * m * n, k * (m*m + n*n)]
                         | n <- [1..x]
                         , m <- [n+1..x]
                         , let (k, r) = x `divMod` (2 * (m*m + m*n))
                         , r == 0
                         , k > 0 ]

main :: IO ()
main = print
     . fst
     . maximumBy (compare `on` snd)
     . map (\x -> (x, Set.size . triples $ x))
     $ [1..1000]
