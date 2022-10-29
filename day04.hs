import Data.List (transpose)
import Util (wordsBy, chunksOf)

main :: IO ()
main = do
  numbersCalled <- fmap (wordsBy (== ',')) getLine
  boards <- fmap (chunksOf 5 . fmap words . filter (/= "") . lines) getContents
  print $ part1 numbersCalled boards
  print $ part2 numbersCalled boards
  return ()

type Board = [[String]]

part1 :: [String] -> [Board] -> Int
part1 (x:xs) boards = let
  boards' = fmap (markBoard x) boards
  in if any checkWin boards' then computeScore (read x) (head $ filter checkWin boards') else part1 xs boards'

part2 :: [String] -> [Board] -> Int
part2 (x:xs) [b] = let
  b' = markBoard x b
  in if checkWin b' then computeScore (read x) b' else part2 xs [b']
part2 (x:xs) boards = let
  boards' = fmap (markBoard x) boards
  in part2 xs (filter (not . checkWin) boards')

computeScore :: Int -> Board -> Int
computeScore x b = let
  remainingTiles = sum . fmap read . filter (/= "") . concat $ b
  in remainingTiles * x

markBoard :: String -> Board -> Board
markBoard n = fmap (fmap (\x -> if  x == n then "" else x))

checkWin :: Board -> Bool
checkWin b
  | any (all (== "")) b = True
  | any (all (== "")) . transpose $ b = True
  | otherwise = False
