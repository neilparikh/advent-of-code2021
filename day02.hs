main :: IO ()
main = do
  input <- fmap (fmap words . lines) getContents
  print $ part1 input (0, 0)
  print $ part2 input (0, 0, 0)

part1 :: [[String]] -> (Int, Int) -> Int
part1 [] (x, y) = x * y
part1 (["forward", amount]:xs) (x, y) = part1 xs (x + read amount, y)
part1 (["up", amount]:xs) (x, y) = part1 xs (x, y - read amount)
part1 (["down", amount]:xs) (x, y) = part1 xs (x, y + read amount)

part2 :: [[String]] -> (Int, Int, Int) -> Int
part2 [] (x, y, c) = x * y
part2 (["forward", amount]:xs) (x, y, c) = part2 xs (x + read amount, y + read amount * c, c)
part2 (["up", amount]:xs) (x, y, c) = part2 xs (x, y, c - read amount)
part2 (["down", amount]:xs) (x, y, c) = part2 xs (x, y, c + read amount)
