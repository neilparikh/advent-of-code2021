import Util
import qualified Data.IntMap.Strict as IM
import Debug.Trace

-- TODO: Just switch to using assoc list instead of bothing
-- with IntMap, since there's only 9 keys

main :: IO ()
main = do
  input <- fmap (fmap read . wordsBy (== ',')) getLine
  print $ length $ applyNTimes 80 sim input
  let startMap = inputToMap input
  print $ IM.foldr (+) 0 $ applyNTimes 256 sim2 startMap

inputToMap :: [Int] -> IM.IntMap Integer
inputToMap ls = foldl (\m x -> IM.adjust (+ 1) x m) (IM.fromList (zip [0..8] (repeat 0))) ls

sim :: [Int] -> [Int]
sim fish = let
  numNew = length . filter (== 0) $ fish
  next n = if n == 0 then 6 else n - 1
  in fmap next fish ++ replicate numNew 8

sim2 :: IM.IntMap Integer -> IM.IntMap Integer
sim2 m = let
  numNew = m IM.! 0
  newM = IM.fromListWith (+) $ fmap (\(k, v) -> (if k == 0 then 6 else k - 1, v)) $ IM.toList m
  in IM.insertWith (+) 8 numNew newM
