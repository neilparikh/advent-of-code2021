import Util
import qualified Data.Map.Strict as M
import Data.Bifunctor (bimap)

main :: IO ()
main = do
  (template, rules) <- fmap (bimap head parseRules . listToTuple . wordsOn "" . lines) (readFile "day14.in")
  print $ solve rules template 10
  print $ solve rules template 40

solve :: M.Map String Char -> String -> Int -> Int
solve rules s numSteps = let
  pairs = fmap (\(x, y) -> [x, y]) $ zip s (tail s)
  pairsMap = foldl (\m x -> M.insertWith (+) x 1 m) M.empty pairs
  pairsMapFinal = applyNTimes numSteps (polymerize rules) pairsMap
  counts = countLetters pairsMapFinal [head s, last s]
  in maximum counts - minimum counts

polymerize :: M.Map String Char -> M.Map String Int -> M.Map String Int
polymerize m p = let
  pairsFor s@[a, b] = let new = m M.! s in [[a, new], [new, b]]
  newPairs = M.fromListWith (+) . concatMap (\(s, n) -> zip (pairsFor s) (repeat n)) . M.toList $ p
  in newPairs

countLetters :: M.Map String Int -> String -> M.Map Char Int
countLetters p [a, b] = let
  counts = M.adjust (+1) b . M.adjust (+1) a . M.fromListWith (+) . concatMap (\([x, y], n) -> [(x, n), (y, n)]) . M.toList $ p
  in fmap (`div` 2) counts

parseRules :: [String] -> M.Map String Char
parseRules = M.fromList . fmap (bimap id head . (\x -> (head x, last x)) . wordsOn ' ')