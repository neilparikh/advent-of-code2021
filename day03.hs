import Data.List
import Data.Ord
import Data.Char (digitToInt)
import Debug.Trace

toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0

main :: IO ()
main = do
  input <- fmap (fmap id . lines) getContents
  print $ part1 input
  let a = toDec $ part2 O2 input
  let b = toDec $ part2 CO2 input
  print $ a * b
  return ()

part1 :: [String] -> Int
part1 = product . fmap toDec . transpose . fmap (fmap head . sortBy (comparing length) . group . sort) . transpose

data Type = O2 | CO2

part2 :: Type -> [String] -> String
part2 _ [x] = x
part2 t xs = x : (part2 t . fmap tail . filter ((== x) . head) $ xs)
  where
  mostCommon = head . last . sortBy (comparing length <> comparing head) . group . sort . fmap head $ xs
  leastCommon = if mostCommon == '1' then '0' else '1'
  x = case t of {O2 -> mostCommon; CO2 -> leastCommon}
