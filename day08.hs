import Util

main :: IO ()
main = do
  input <- fmap (fmap (wordsBy (== "|") . words) . lines) getContents
  print $ solve1 input

solve1 :: [[[String]]] -> Int
solve1 = sum . concatMap (fmap (const 1) . filter ((`elem` [2, 4, 3, 7]) . length). (!! 1))
