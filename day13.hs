import Util
import Data.Bifunctor (bimap)
import Data.List (nub, transpose)

main :: IO ()
main = do
  (points, folds) <- fmap (bimap parsePoints parseFolds . listToTuple . wordsBy (== "") . lines) getContents
  print $ part1 points folds
  putStr $ part2 points folds

part1 :: [(Int, Int)] -> [(String, Int)] -> Int
part1 points folds = length . nub . applyFold points . head $ folds

part2 :: [(Int, Int)] -> [(String, Int)] -> String
part2 points folds = let
  dots = nub $ foldl applyFold points folds
  (maxX, maxY) = bimap maximum maximum . unzip $ dots
  grid = transpose . chunksOf (maxY + 1) $ prod [0..maxX] [0..maxY]
  in plot dots grid

applyFold :: [(Int, Int)] -> (String, Int) -> [(Int, Int)]
applyFold xs (dir, n) = fmap foldPt xs
  where
  foldPt (x, y) = case dir of
    "x" -> (if x < n then x else x - 2 * (x - n), y)
    "y" -> (x, if y < n then y else y - 2 * (y - n))
    _ -> error "bad input"

plot :: [(Int, Int)] -> [[(Int, Int)]] -> String
plot dots grid = unlines $ fmap (fmap (\pt -> if pt `elem` dots then '#' else '.')) grid

parsePoints :: [String] -> [(Int, Int)]
parsePoints = fmap (listToTuple . fmap read . wordsBy (== ','))

parseFolds :: [String] -> [(String, Int)]
parseFolds = fmap (bimap id read . listToTuple . wordsBy (== '=') . last . words)
