import Util
import qualified Data.Map.Strict as M
import Data.List (permutations, sort)

digitShapes :: M.Map String Char
digitShapes = M.fromList [
  ("abcefg", '0'),
  ("cf", '1'),
  ("acdeg", '2'),
  ("acdfg", '3'),
  ("bcdf", '4'),
  ("abdfg", '5'),
  ("abdefg", '6'),
  ("acf", '7'),
  ("abcdefg", '8'),
  ("abcdfg", '9')
  ]

main :: IO ()
main = do
  input <- fmap (fmap (wordsBy (== "|") . words) . lines) getContents
  print $ sum . fmap solve1 $ input
  print $ sum . fmap solve2 $ input

solve1 :: [[String]] -> Int
solve1 = sum . fmap (const 1) . filter ((`elem` [2, 4, 3, 7]) . length) . (!! 1)

solve2 :: [[String]] -> Int
solve2 [patterns, output] = let
  possibleMaps = fmap (M.fromList . flip zip "abcdefg") . permutations $ "abcdefg"
  patternPossible mapping = (`elem` (M.keys digitShapes)) . sort . fmap ((M.!) mapping)
  mappingPossible mapping = all (patternPossible mapping) patterns
  [validMapping] = filter mappingPossible possibleMaps
  in read . fmap ((M.!) digitShapes . sort . fmap ((M.!) validMapping)) $ output