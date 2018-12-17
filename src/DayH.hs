{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall   #-}

module DayH where

import Control.Applicative
import Control.Monad
import Data.Array
import Data.Maybe
import Text.ParserCombinators.ReadP

import Debug.Trace

import Lib

type Coord = (Int, Int)
data Vein = Vein { _vx :: Coord, _vy :: Coord } -- x-span, y-span, inclusive
  deriving Show

parseVein :: ReadP (Char, Int, Int)
parseVein = do
  t <- get
  void $ char '='
  start <- digit
  end <- (string ".." >> digit) <|> pure start
  return (t, start, end)

parseLine :: ReadP Vein
parseLine = do
  (t1, s1, e1) <- parseVein
  void $ string ", "
  (_, s2, e2) <- parseVein
  eof
  pure $ if t1 == 'x'
    then Vein (s1, e1) (s2, e2)
    else Vein (s2, e2) (s1, e1)

data Block = Water | Sand | Clay deriving Show
type G = Array Coord Block

readGame :: [Vein] -> G
readGame vs = foldr addVein emptyArr vs
  where
    addVein (Vein (x1, x2) (y1, y2)) =
      ( // ((,Clay) <$> [(y, x) | y <- [y1..y2], x <- [x1..x2]]))
    emptyArr = listArray ((0, xmin), (ymax, xmax)) $ repeat Sand
    extremes = minmax . concatMap (\(a, b) -> [a, b]) ...  map
    (xmin, xmax) = extremes _vx vs
    (_, ymax) = extremes _vy vs

dayH :: IO ()
dayH = do
  input <- lines  <$> readFile "data/dayH.txt"
  -- print $ traverse (parse parseLine) input
  let game = readGame . fromJust . traverse (parse parseLine) $ input
  print $ bounds game
  print . take 100 $ assocs game
