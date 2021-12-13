{-# LANGUAGE TupleSections #-}
import Util
import Data.Maybe (catMaybes)
import Data.List (partition, sort)

main :: IO ()
main = do
  input <- fmap (fmap id . lines) getContents
  let tillEnd = fmap (farthest f . ("",)) input
  let (incomplete, corrupt) = partition (null . snd) tillEnd
  print $ part1 corrupt
  print $ part2 incomplete

--    Stack , Input
f :: (String, String) -> Maybe (String, String)
f (_, []) = Nothing
f ([], y:ys) = if isOpen y then Just (pure y, ys) else Nothing
f (x:xs, y:ys)
  | isOpen y = Just (y:x:xs, ys)
  | x == closeToOpen y = Just (xs, ys)
  | otherwise = Nothing

part1 :: [(String, String)] -> Int
part1 = sum . fmap (score . head . snd)
  where
  score c = case c of
    ')' -> 3
    ']' -> 57
    '}' -> 1197
    '>' -> 25137
    _ -> error "Should not happen"

part2 :: [(String, String)] -> Int
part2 = median . sort . fmap (score . fmap openToClose . fst)
  where
  score = foldl (\acc x -> acc * 5 + scorePer x) 0
  scorePer c = case c of
    ')' -> 1
    ']' -> 2
    '}' -> 3
    '>' -> 4
    _ -> error "Should not happen"

isOpen :: Char -> Bool
isOpen = flip elem "<{(["

closeToOpen :: Char -> Char
closeToOpen c = case c of
  '>' -> '<'
  ']' -> '['
  ')' -> '('
  '}' -> '{'
  _ -> error "Should not happen"

openToClose :: Char -> Char
openToClose c = case c of
  '<' -> '>'
  '[' -> ']'
  '(' -> ')'
  '{' -> '}'
  _ -> error "Should not happen"
