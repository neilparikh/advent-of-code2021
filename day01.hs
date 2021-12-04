main :: IO ()
main = do
  input <- fmap (fmap read . lines) getContents
  print $ solve 1 input
  print $ solve 3 input

solve :: Int -> [Int] -> Int
solve n list = sum $ zipWith posDiff list (drop n list)
  where
  posDiff a b = if b > a then 1 else 0
