import Util
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Char (isLower)
import Data.Tuple (swap)
import Data.List (sortOn, nub)

main :: IO ()
main = do
  input <- fmap (fmap (listToTuple . fmap stringToNode . wordsOn '-') . lines) (readFile "day12.in")
  let input' = M.fromList . fmap (\ls -> (fst . head $ ls, fmap snd ls)) . groupOn fst . sortOn fst . nub $ input ++ fmap swap input
  print $ go input' S.empty True Start
  print $ go input' S.empty False Start
  return ()

data Node = Lower Char | Upper Char | Start | End
  deriving (Eq, Ord, Show)

stringToNode :: String -> Node
stringToNode "start" = Start
stringToNode "end" = End
stringToNode s | isLower (head s) = Lower (head s)
               | otherwise = Upper (head s)

go :: M.Map Node [Node] -> S.Set Node -> Bool -> Node -> Int
go m s dup End = 1
go m s dup Start | Start `S.member` s = 0
go m s dup Start = let
  s' = S.insert Start s
  newNodes = m M.! Start
  in sum . map (go m s' dup) $ newNodes
go m s dup x@(Upper _) = let newNodes = m M.! x in sum . map (go m s dup) $ newNodes
go m s dup x@(Lower _)
  | x `S.member` s && dup = 0
  | otherwise = let
    s' = S.insert x s
    newNodes = m M.! x
    dup' = dup || x `S.member` s
    in sum . map (go m s' dup') $ newNodes
