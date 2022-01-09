main :: IO ()
main = do
  input <- fmap (fmap id . lines) getContents
  return ()
