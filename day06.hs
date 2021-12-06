import Util
import qualified Data.IntMap.Strict as IM
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
  input <- fmap (fmap read . wordsBy (== ',')) getLine
  print $ length $ applyNTimes 80 sim input
  let startMap = inputToMap input
  print $ IM.foldr (+) 0 $ applyNTimes 256 sim2 startMap

inputToMap :: [Int] -> IM.IntMap Integer
inputToMap ls = foldl (\m x -> IM.insertWith (+) x 1 m) IM.empty ls

sim :: [Int] -> [Int]
sim fish = let
  numNew = length . filter (== 0) $ fish
  next n = if n == 0 then 6 else n - 1
  in fmap next fish ++ replicate numNew 8

sim2 :: IM.IntMap Integer -> IM.IntMap Integer
sim2 m = let
  newFish = fromMaybe 0 . IM.lookup 0 $ m
  m' = IM.mapKeysWith (+) (\k -> if k == 0 then 6 else k - 1) m
  in IM.insert 8 newFish m'
