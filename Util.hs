module Util where
import Control.Arrow ((&&&))
import Control.Monad ((>=>))
import Data.Function (on)
import Data.List (groupBy)

wordsBy :: (a -> Bool) -> [a] -> [[a]]
wordsBy f s = case dropWhile f s of
  [] -> []
  s' -> w : wordsBy f s''
    where (w, s'') = break f s'

wordsOn :: (Eq a) => a -> [a] -> [[a]]
wordsOn x = wordsBy (== x)

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

applyNTimesM :: (Monad m) => Int -> (a -> m a) -> a -> m a
applyNTimesM n f = foldr (>=>) return (replicate n f)

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

converge :: (Eq a) => (a -> a) -> a -> a
converge f x = let x' = f x in (if x == x' then x else converge f x')

groupOn :: (Eq b) => (a -> b) -> [a] -> [[a]]
groupOn f = groupBy ((==) `on` f)