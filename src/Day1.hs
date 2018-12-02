module Day1 where

import Data.Foldable
import Data.Maybe
import Data.Monoid
import Text.Read

parse :: String -> Maybe Int
parse ('+': num) = readMaybe num
parse num = readMaybe num

day1 :: IO ()
day1 = do
  i <- lines <$> readFile "data/day1.txt"
  print $ sum (catMaybes (parse <$> i))
