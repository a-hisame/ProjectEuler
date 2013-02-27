import Data.List
import Control.Applicative

solve052 :: Int -> Int
solve052 k = head $ filter (\x -> minTimesOf k x == 1) [1..]
 where 
  minTimesOf n x = length . nub . map (sort . show) $ (*) <$> [1..n] <*> [x]
