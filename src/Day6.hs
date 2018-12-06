{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall #-}

module Day6 where

import Control.Arrow hiding (arr)
import Data.Array
import Data.Char
import Data.Function
import Data.Maybe
import Data.List
import Data.Semigroup

import Text.ParserCombinators.ReadP

type Point = (Int, Int)

parser :: ReadP Point
parser =
  let digit = read <$> many1 (satisfy isDigit) in
  (,) <$> digit <* string ", " <*> digit <* skipSpaces <* eof

parse :: String -> Maybe Point
parse = readP_to_S parser >>> \case
  [(p, "")] -> Just p
  _ -> Nothing

minmax :: Foldable f => f Int -> (Int, Int)
minmax = (getMin *** getMax) . foldMap (Min &&& Max)

strictMinimumBy :: Ord a1 => (a2 -> a1) -> [a2] -> Maybe a2
strictMinimumBy key =
  sortBy (compare `on` key) >>> \case
    (x:y:_) | key x == key y -> Nothing
    (x:_) -> Just x
    _ -> Nothing

findClosest :: (Ord a, Num a) => [(a, a)] -> (a, a) -> Maybe Char
findClosest points (x, y) =
    fmap snd
    $ strictMinimumBy fst
    $ fmap (first $ distance (x, y))
    $ zip points (['a'..'z'] <> ['A'..'Z'])

distance :: Num a => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

corners :: (Num a, Enum t, Enum a) => ((t, a), (t, a)) -> [(t, a)]
corners ((x1, y1), (x2, y2)) = mconcat [
  [(x, y1) | x <- [x1..x2]],
  [(x, y2) | x <- [x1..x2]],
  [(x1, y) | y <- [y1+1..y2-1]],
  [(x2, y) | y <- [y1+1..y2-1]]]

sumDistance :: [Point] -> Point -> Int
sumDistance points o = sum $ distance o <$> points

day6 :: IO ()
day6 = do
  input <- lines <$> readFile "data/day6.txt"
  -- let input = ["1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9"]
  let points = fromJust $ traverse parse input

  -- find the bounding box of the points
  let (x1, x2) = minmax $ map fst points
  let (y1, y2) = minmax $ map snd points
  let b = ((x1, y1), (x2, y2))

  -- for each x, y find the closest point
  let arr = listArray b (findClosest points <$> range b)

  -- delete the things at the corners
  let toDelete = nub $ (arr !) <$> corners b
  let clean x = if x `elem` toDelete then Nothing else x
  let cleanArr = clean <$> arr

  -- find the biggest thing
  let res = maximum $ map length $ group $ sort $ catMaybes $ elems cleanArr
  putStrLn $ "Part 1: " <> show res

  -- and make nice debug file
  let rows = (map.map) (fromMaybe '.' . snd ) $ groupBy ((==) `on` (fst.fst)) $ assocs cleanArr
  writeFile "/tmp/outputs" $ unlines rows

  let distanceSums = sumDistance points <$> range b
  putStrLn $ "Part 1: " <> (show . length . filter (<10000) $ distanceSums)
