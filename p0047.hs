import Data.List
import Control.Applicative

-- Project Euler 047 solver
solve047 :: Int -> Maybe Int
solve047 n = (+1) <$> indexOf (replicate n n) pss
 where 
  pss = map (length . factors) [1..]

-- リストの中で最初の部分リストが出現するindexを返す
indexOf :: (Eq a) => [a] -> [a] -> Maybe Int
indexOf xs = findIndex (isPrefixOf xs) . tails

-- 素数のリスト
primes :: [Integer]
primes = p' [2..]
 where 
  p' (p:xs) = p:(p' $ filter (\x -> x `mod` p /= 0) xs)

-- 素因数分解
factors :: Integer -> [(Integer, Int)] 
factors x 
 | x <= 0 = []
 | x == 1 = [(1,1)]
 | otherwise = map (\xs -> (head xs, length xs)) . group $ factors' x 

-- 素因数一覧を生成
factors' :: Integer -> [Integer]
factors' n = sort $ f n []
 where
  f n xs = case divs n of 
   [] -> xs 
   otherwise -> f (foldr (flip div) n (divs n)) $ (divs n) ++ xs

-- nを割り切れる全ての素数を取得
divs :: Integer -> [Integer]
divs n = filter (\x -> n `mod` x == 0) $ takeWhile (<= n) primes

