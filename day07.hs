import Util

main :: IO ()
main = do
  input <- fmap (fmap read . wordsBy (== ',')) getLine
  let possibleLocations = [(minimum input)..(maximum input)]
  print $ minimum $ fmap (fuelReq id input) possibleLocations
  print $ minimum $ fmap (fuelReq sumTo input) possibleLocations
  return ()

fuelReq :: (Int -> Int) -> [Int] -> Int -> Int
fuelReq cost xs n = sum $ fmap (cost . abs . (subtract n)) xs

sumTo :: Int -> Int
sumTo x = (x * (x + 1)) `div` 2
