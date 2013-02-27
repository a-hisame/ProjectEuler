import Data.List
import Control.Applicative

solve053 :: Int 
solve053 = length . filter (\(n, r) -> (comb n r) >= 1000000) $ [(n, r) | n <- [1..100], r <- [1..n]]

comb :: Integer -> Integer -> Integer
comb n r = div (fact n) ((fact r) * (fact (n-r)))
  
fact :: Integer -> Integer
fact n
 | n <= 0 = 1
 | otherwise = product [1..n]

