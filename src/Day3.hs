{-# LANGUAGE PartialTypeSignatures #-}

module Day3 where

import Control.Monad (when)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (Sum(..))
import Data.List (maximumBy, elemIndex)
import Text.Read
import Debug.Trace

type Claim = ((Int, Int) -> Bool)
between x dx a = a < (x + dx) && a >= x
(...) = (.) . (.)

claim :: (Int, Int, Int, Int) -> Claim
claim (x, y, dx, dy) = \(a, b) -> between x dx a && between y dy b

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

day3 :: IO ()
day3 = do
  inputs <- lines <$> readFile "data/day3.txt"
  let parsed = traverse parse inputs
  case parsed of
    Just c -> do
      let maxX = maximum $ (\(x, _, dx, _) -> x + dx) <$> c
      let maxY = maximum $ (\(_, y, _, dy) -> y + dy) <$> c
      let claims = ((Sum . fromEnum) ... claim) <$> c
      let foo = length
                $ filter ((>=2) .getSum)
                [foldMap ($ (x, y)) claims
                  | x <- [0..maxX + 1]
                  , y <- [0..maxY + 1]]
      print foo
    _ -> print "no parse"
