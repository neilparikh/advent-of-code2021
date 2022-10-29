import Data.Map.String as M

main :: IO ()
main = do
  input <- parseInput <$> getContents
  print input
  return ()

parseInput :: String -> [[Int]]
parseInput = fmap (fmap (read . (\x -> [x]))) . lines
