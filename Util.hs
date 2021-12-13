module Util where
import Control.Arrow ((&&&))

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy f s = case dropWhile f s of
  [] -> []
  s' -> w : wordsBy f s''
    where (w, s'') = break f s'

chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n l
  | n > 0 = take n l : chunksOf n (drop n l)
  | otherwise = error "Negative or zero n"

listToTuple :: [a] -> (a, a)
listToTuple [x, y] = (x, y)
listToTuple _ = error "list does not have exactly 2 elems"

prod :: [a] -> [b] -> [(a, b)]
prod a b = [(x, y) | x <- a, y <- b]

applyNTimes :: Int -> (a -> a) -> a -> a
applyNTimes n f = foldr (.) id (replicate n f)

fmapWithTag :: Functor f => (a -> b) -> f a -> f (a, b)
fmapWithTag f = fmap (id &&& f)

-- https://hackage.haskell.org/package/zippers-0.3.2/docs/src/Control.Zipper.Internal.html#farthest
farthest :: (a -> Maybe a) -> a -> a
farthest f = go where
  go a = maybe a go (f a)

safeHead :: [a] -> Maybe a
safeHead []    = Nothing
safeHead (x:_) = Just x

median :: [a] -> a
median xs
  | null xs = error "Can't find median of empty list"
  | even (length xs) = error "Can't find median of even length list"
  | otherwise = let
    l = length xs
    m = l `div` 2
    in xs !! m
