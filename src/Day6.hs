{-# LANGUAGE PartialTypeSignatures #-}
module Day6 where

import Control.Arrow
import Data.Array
import Data.Char
import Data.Function
import Data.Ord
import Data.Maybe
import Data.List
import Data.Semigroup
import Data.Tuple

import Debug.Trace

import Text.ParserCombinators.ReadP
import Text.Read

type Point = (Int, Int)

parser :: ReadP Point
parser =
  let digit = read <$> many1 (satisfy isDigit) in
  (,) <$> digit <* string ", " <*> digit <* skipSpaces <* eof

parse str = case readP_to_S parser str of
  [(p, "")] -> Just p
  _ -> Nothing

minmax :: Foldable f => f Int -> (Int, Int)
minmax = (getMin *** getMax) . foldMap (Min &&& Max)

-- strictMinimumBy :: (Show a, Ord a) => [(a, b)] -> Maybe (a, b)
strictMinimumBy key xs = let sr = sortBy (compare `on` key) xs
  in
  case sr of
  (x:y:_) | key x == key y -> Nothing
  (x:xs) -> Just x
  _ -> Nothing

findClosest points (x, y) = fmap snd $ strictMinimumBy fst $ fmap (first $ distance (x, y)) $ zip points ['a'..]
distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

-- addPointsToArray :: Array Point Int -> [Point] -> Array Point Int
-- addPointsToArray arr points =
--   let findClosest :: Point -> Int
--       findClosest (x, y) = minimum $ fmap (distance (x, y)) points
--       distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (x2 - x1)
--   in fmap _ arr

corners ((x1, y1), (x2, y2)) = [(x, y) | x <- [x1, x2], y <- [y1, y2]]

day6 = do
  input <- lines <$> readFile "data/day6.txt"
  let input = ["1, 1", "1, 6", "8, 3", "3, 4", "5, 5", "8, 9"]
  let points = fromJust $ traverse parse input

  -- find the bounding box of the points
  let (x1, x2) = minmax $ map fst points
  let (y1, y2) = minmax $ map snd points
  let b = ((x1, y1), (x2, y2))

  -- extend it arbitrarity on all sides to find
  -- points that spill out to the sides
  let (w, h) = ((x2 - x1), (y2 - y1))
  let extended = ((x1 - w, y1 - h), (x2 + w, y2 + h))

  -- for each x, y find the closest point
  let arr = listArray extended (findClosest points <$> range extended)

  -- delete the things at the corners
  let toDelete = (arr !) <$> corners extended
  let clean = \x -> if x `elem` toDelete then Nothing else x
  let cleanArr = clean <$> arr

  -- find the biggest thing
  let res = maximum $ map length $ group $ catMaybes $ elems cleanArr
  print res
  let withPoints = ixmap ((swap *** swap) $ bounds cleanArr) swap $ cleanArr // (zip points (Just <$> ['A'..]))
  let rows = (map.map) (fromMaybe '.') $ (map.map) snd $ groupBy ((==) `on` (fst.fst)) $ assocs withPoints
  print `mapM_` rows
