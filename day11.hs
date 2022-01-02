import Util
import Data.Monoid (Sum (Sum), getSum)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

type OctoMap = M.Map (Int, Int) Int

main :: IO ()
main = do
  input <- fmap (fmap (fmap (read . pure)) . lines) (readFile "day11.in")
  let pointsWithCoords = concat (zipWith (\x ls -> fmap (\(y, p) -> ((x, y), p)) ls) [0..] (fmap (zip [0..]) input))
  let octopusMap = M.fromList pointsWithCoords
  print $ part1 octopusMap
  print $ part2 octopusMap

part1 :: OctoMap -> Int
part1 = getSum . fst . applyNTimesM 100 step -- Monad instance for (l,) is basically Writer Monad

part2 :: OctoMap -> Int
part2 m = let
  (Sum c, m') = step m
  in if c == M.size m then 1 else 1 + part2 m'

step :: OctoMap -> ((Sum Int), OctoMap)
step m = let
  m' = fmap (+1) m
  (s, m'') = converge subStep (S.empty, m')
  m''' = fmap (\v -> if v > 9 then 0 else v) m''
  in (Sum $ S.size s, m''')
  where
  subStep :: (S.Set (Int, Int), OctoMap) -> (S.Set (Int, Int), OctoMap)
  subStep (s, m) = let
    toFlash = M.keys . M.filterWithKey (\k v -> v > 9 && k `S.notMember` s) $ m 
    adjLocs (x, y) = [(x-1, y), (x+1, y), (x, y-1), (x, y+1), (x-1, y-1), (x-1, y+1), (x+1, y-1), (x+1, y+1)]
    adj = concatMap adjLocs toFlash
    s' = S.union s . S.fromList $ toFlash
    m' = foldl (\acc p -> M.adjust (+1) p acc) m adj
    in (s', m')