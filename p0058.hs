-- http://projecteuler.net/problem=58  
import Data.Numbers.Primes

-- 右斜め上の対角線の数値を求める (n = 1の時、中心の1を求める)
rightUpper :: Int -> Int
rightUpper n = head . drop (n-1) . scanl (+) 1 $ [2,10..]

-- 引数nに対して、2n-1のサイズの正方形の４隅の数値を得る
toAngle :: Int -> [Int]
toAngle n = [v, v+2*n', v+4*n', v+6*n']
 where
  n' = n - 1
  v = rightUpper n 

-- toAngleで得た数値のうち、(素数の個数, 4)のタプルを返す
includePrime :: Int -> (Int, Int)
includePrime 1 = (0, 1)
includePrime n = (length . filter isPrime . toAngle $ n, 4)

-- (x, y)のタプルから x/y を求める
rate :: (Int, Int) -> Double
rate (x, y) = (fromIntegral x) / (fromIntegral y)

-- 2タプルの和を求める
tupleSum :: (Int, Int) -> (Int, Int) -> (Int, Int)
tupleSum (a, b) (c, d) = (a+c, b+d)

-- 補助解。0.1を初めて下回るrightUpperの引数nを求める
solveN :: Int
solveN = (2+) . length . takeWhile ( > 0.1) . map rate . drop 2 . scanl tupleSum (0, 0) . map includePrime $ [1..]

-- 解答. 2n - 1を算出する
result :: Int
result = 2 * solveN - 1


