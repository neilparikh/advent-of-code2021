import Util
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Char (isLower)
import Data.Tuple (swap)
import Data.List (sortOn, nub)

main :: IO ()
main = do
  input <- fmap (fmap (listToTuple . wordsOn '-') . lines) (readFile "day12.in")
  let input' = M.fromList . fmap (\ls -> (fst . head $ ls, fmap snd ls)) . groupOn fst . sortOn fst . nub $ input ++ fmap swap input
  print $ go input' S.empty True "start"
  print $ go input' S.empty False "start"
  return ()

go :: M.Map String [String] -> S.Set String -> Bool -> String -> Int
go m s dup "end" = 1
go m s dup x 
  | all isLower x && x `S.member` s && (dup || x == "start") = 0
  | otherwise = let
    s' = S.insert x s
    newNodes = m M.! x
    dup' = dup || (x `S.member` s && all isLower x)
    in sum . map (go m s' dup') $ newNodes