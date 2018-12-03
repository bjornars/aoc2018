{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}

module Day3 where

import Data.Array

import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (Sum(..))
import Data.List (maximumBy, elemIndex)
import Text.Read
import Debug.Trace

(...) = (.) . (.)

claimToList (x, y, dx, dy) = (, Sum 1) <$> range ((x, y), (x+dx-1, y+dy-1))

parse :: String -> Maybe _
parse line = case words line of
    [_, _, start, size] -> parseCoords (init start) size
    _ -> Nothing

splitOn :: Char -> String -> _
splitOn c str = (\idx -> (take idx str, drop (idx + 1) str))
                <$> (elemIndex c str)

parseCoords :: String -> String -> _
parseCoords start size = do
  (x, y) <- splitOn ',' start
  (dx, dy) <- splitOn 'x' size
  let r = readMaybe :: String -> Maybe Int
  (,,,) <$> r x <*> r y <*> r dx <*> r dy

fillArray :: Array (Int, Int) (Sum Int) -> [_] -> Array (Int, Int) (Sum Int)
fillArray arr [] = arr
fillArray arr (c:cs) =
  fillArray (accum (<>) arr (claimToList c)) cs

day3 :: IO ()
day3 = do
  inputs <- lines <$> readFile "data/day3.txt"
  let parsed = traverse parse inputs
  case parsed of
    Just c -> do
      let maxX = maximum $ (\(x, _, dx, _) -> x + dx) <$> c
      let maxY = maximum $ (\(_, y, _, dy) -> y + dy) <$> c
      let arr :: Array (Int, Int) (Sum Int)
          arr = listArray ((0, 0), (maxX, maxY)) $ repeat (Sum 0)
      let filled = fillArray arr c
      -- print arr
      print $ length $ filter ((>=2) . getSum) $ elems filled
    _ -> print "no parse"
