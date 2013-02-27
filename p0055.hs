import Control.Applicative

-- 解答
result :: Int
result = length . filter lychrel $ map toInteger [0..9999]

-- Lychrel数の判定を行う
-- ０番目の要素が回文である場合でも、Lychrel数の可能性がある
lychrel :: Integer -> Bool
lychrel = (==) maxNum . cycleLength
 where
  maxNum = 50 - 1 
  cycleLength = length . take maxNum . lychrelList
  lychrelList = takeWhile (not . palindromic) . iterate nextLychrel . nextLychrel

-- Lychrel数判定の次の数値を返す
nextLychrel :: Integer -> Integer
nextLychrel = (+) <$> id <*> (read . reverse . show)

-- 回文数字であればtrueを返す
palindromic :: Integer -> Bool 
palindromic n = (==) numlength palindlength 
 where
  f = zip <$> id <*> reverse
  xs = show n
  numlength = length xs
  palindlength = length . filter (\(x,y) -> x == y) $ f xs


