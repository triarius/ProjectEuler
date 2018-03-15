import qualified Data.Set as S

powers :: Integer -> [Integer]
powers n = (^) <$> [2..n] <*> [2..n]

main :: IO ()
main = print $ S.size . S.fromList . powers $ 100
