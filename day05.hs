import qualified Data.Map.Strict as M
import Util (wordsBy, listToTuple)

main :: IO ()
main = do
  allLines <- fmap (fmap parseLine . lines) getContents
  print $ solve . filter isStraight $ allLines
  print $ solve allLines
  return ()

type Point = (Int, Int)
type Line = ((Int, Int), (Int, Int))
type Grid = M.Map Point Int

solve :: [Line] -> Int
solve = M.size . M.filter (> 1) . foldl markGrid M.empty . fmap genPoints

markGrid :: Grid -> [Point] -> Grid
markGrid = foldl (\m p -> M.insertWith (+) p 1 m)

genPoints :: Line -> [Point]
genPoints ((x1, y1), (x2, y2)) = zip (genLine x1 x2) (genLine y1 y2)

isStraight :: Line -> Bool
isStraight ((x1, y1), (x2, y2)) = x1 == x2 || y1 == y2

genLine :: Int -> Int -> [Int]
genLine a b
  | a < b = [a..b]
  | a == b = repeat a
  | otherwise = reverse [b..a]

parseLine :: String -> Line
parseLine s = let
  [posA, _, posB] = words s
  in listToTuple . fmap (listToTuple . fmap read . wordsBy (== ',')) $ [posA, posB]
