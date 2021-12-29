import Util
import Data.Maybe (catMaybes)
import Data.List (sort)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace

t :: (Show a) => a -> a
t = traceShowId

main :: IO ()
main = do
  input <- fmap (fmap (fmap (read . pure)) . lines) (readFile "day09.in")
  let pointsWithCoords = concat (zipWith (\x ls -> fmap (\(y, p) -> ((x, y), p)) ls) [0..] (fmap (zip [0..]) input))
  let heightMap = M.fromList pointsWithCoords
  let lowPts = M.filterWithKey (isLow heightMap) $ heightMap
  print $ sum . fmap succ . M.elems $ lowPts
  print $ product . take 3 . reverse . sort . fmap (basinSize heightMap) . M.keys $ lowPts
  return ()

isLow :: M.Map (Int, Int) Int -> (Int, Int) -> Int -> Bool
isLow m (x, y) h = let
  adjLocs = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
  adjHeights = catMaybes . fmap (flip M.lookup m) $ adjLocs
  in all (> h) adjHeights

basinSize :: M.Map (Int, Int) Int -> (Int, Int) -> Int
basinSize m p@(x, y) = let
  h = m M.! p
  adjLocs = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
  next = adjLocs
  nextToVisit = zip (repeat (m M.! (x, y))) next
  nextSeen = S.fromList (zip (repeat p) next)
  in go nextToVisit nextSeen S.empty
  where
  go :: [(Int, (Int, Int))] -> S.Set ((Int, Int), (Int, Int)) -> S.Set (Int, Int) -> Int
  go [] _ basin = S.size basin + 1
  go ((h, p@(x, y)):xs) seen basin = let
    h' = m M.! p
    isInBasin = M.member p m && h' > h && h' /= 9
    adjLocs = [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]
    next = filter (\p' -> S.notMember (p, p') seen) adjLocs
    nextToVisit = xs ++ zip (repeat h') next
    nextSeen = foldr S.insert seen (zip (repeat p) next)
    in if isInBasin then go nextToVisit nextSeen (S.insert p basin)
                    else go xs seen basin