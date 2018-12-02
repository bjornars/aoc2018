module Day2 where

import Data.List
import Data.Monoid

checksums :: String -> (Sum Int, Sum Int)
checksums f =
  let counts = map length . group . sort $ f
      has = Sum . fromEnum . (`elem` counts)
  in (has 2, has 3)

dropLetter :: Int -> [a] -> [a]
dropLetter idx xs = let
  tail' (x:xs) = xs
  tail' _ = []
  (heads, tails) = splitAt idx xs
  in heads <> tail' tails

offByOneFor :: Eq a => [[a]] -> Int -> [[a]]
offByOneFor xs n = let xs' = dropLetter n <$> xs in
 xs' \\ (nub xs')

offByOne :: Eq a => [[a]] -> [[a]]
offByOne xs = let
  len = length $ head xs
  in concatMap (offByOneFor xs) [0..len]

day2 :: IO ()
day2 = do
  i <- lines <$> readFile "data/day2.txt"
  let (Sum doubles, Sum triples) = mconcat $ checksums <$> i
  print (doubles * triples)
  print (intercalate ", " $ offByOne i)
