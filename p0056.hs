-- http://projecteuler.net/problem=56
import Data.Char

-- 解答
result :: Int
result = maximum . map sumDigit $ [a ^ b | a <- [0..99], b <- [0..99]]
 where
  sumDigit n = sum . map digitToInt $ show n 
