-- import Data.Map.String as M
import Util (safeAt)
import Data.Maybe (catMaybes)
import Control.Monad.Identity

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print $ subtract (input !! 0 !! 0) <$> (runIdentity $ part1 input (0, 0))
  return ()

parseInput :: String -> [[Int]]
parseInput = fmap (fmap (read . (: []))) . lines

part1 :: (Monad m) => [[Int]] -> (Int, Int) -> m (Maybe Int)
part1 map (x, y)
  | x >= (length (map !! 0)) || y >= length map = pure Nothing
  | otherwise = do
      r <- part1 map (x + 1, y)
      d <- part1 map (x, y + 1)
      let next = catMaybes [r, d]
      let nextCost = if (length next > 0) then minimum next else 0
      pure $ Just $ ((map !! y) !! x) + nextCost