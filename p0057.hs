-- http://projecteuler.net/problem=57
import Data.Ratio
import Control.Applicative

-- 解答を算出する
result :: Int
result = length . filter ((>) <$> digitLength . numerator <*> digitLength . denominator) . map root2' $ [0..999]

-- 何桁で表現されているかを返す
digitLength :: Show a => a -> Int
digitLength = length . show

-- nで近似される2の平方根を分数型で返す
root2' :: Integer -> Ratio Integer
root2' n = 1 + rdiv 1 (rt n)
 where
  rdiv a r = (a * denominator r) % (numerator r)
  rt 0 = 2 % 1
  rt k = 2 + rdiv 1 (rt (k-1))
