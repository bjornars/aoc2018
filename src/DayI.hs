{-# OPTIONS_GHC -Wall   #-}

module DayI where

import Control.Monad
import Data.Array

data Cell = Tree | Mill | Open deriving (Ord, Eq, Show)

parse :: Char -> Cell
parse '|' = Tree
parse '.' = Open
parse '#' = Mill
parse _ = undefined

type Coords = (Int, Int)
type Game = Array Coords Cell

count :: Cell -> [Cell] -> Int
count cell = length . filter (==cell)

rule :: Cell -> [Cell] -> Cell
rule Open c | count Tree c >= 3 = Tree
rule Tree c | count Mill c >= 3 = Mill
rule Mill c | count Tree c == 0  || count Mill c == 0 = Open
rule cell _ = cell

gen :: Game -> Game
gen cells = let
  b = bounds cells
  (maxY, maxX) = snd b
  adjs (iy, ix) = [(y, x)
                  | y <- [iy-1..iy+1]
                  , x <- [ix-1..ix+1]
                  , x > 0, x <= maxX
                  , y > 0, y <= maxY
                  , (y,x) /= (iy,ix)]
  step yx = rule (cells ! yx) ((cells !) <$> (id (adjs yx)))
  in listArray b (step <$> indices cells)

pprint :: Int -> [Cell] -> IO ()
pprint width cs = do
  let toP Open = '.'
      toP Tree = '|'
      toP Mill = '#'
  putStrLn (toP <$> take width cs)
  let rest = drop width cs
  unless (null rest) $ pprint width rest

dayI :: IO ()
dayI = do
  input <- lines <$> readFile "data/dayI.txt"
  let input1 = [".#.#...|#.",
               ".....#|##|",
               ".|..|...#.",
               "..|#.....#",
               "#.#|||#|#|",
               "...#.||...",
               ".|....|...",
               "||...#|.#|",
               "|.||||..|.",
               "...#.|..|."
               ]
  let cells = concat . (map.map) parse $ input
  let game = listArray ((1, 1), (length input, length (head input))) cells
  pprint (length (head input)) $ elems game

  let res = iterate gen game !!  10
  print $ replicate 20 '*'
  pprint (length (head input)) $ elems res
  putStrLn $ "Part1: " <> show (count Tree (elems res) * count Mill (elems res))
