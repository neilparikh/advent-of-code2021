-- import Data.Map.String as M
import Util (safeAt)
import Data.Maybe (catMaybes)

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print $ subtract (input !! 0 !! 0) <$> part1 input (0, 0)
  return ()

parseInput :: String -> [[Int]]
parseInput = fmap (fmap (read . (: []))) . lines

part1 :: [[Int]] -> (Int, Int) -> Maybe Int
part1 map (x, y)
  | x >= (length (map !! 0)) || y >= length map = Nothing
  | otherwise = Just $ ((map !! y) !! x) + nextCost
  where
  next = catMaybes [part1 map (x + 1, y), part1 map (x, y + 1)]
  nextCost = if (length next > 0) then minimum next else 0