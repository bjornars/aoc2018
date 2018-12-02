module Day1 where

import Data.Maybe
import Data.IntSet as I
import Text.Read

parse :: String -> Maybe Int
parse ('+': num) = readMaybe num
parse num = readMaybe num

dup xs = go xs (I.singleton 0) 0
  where
    go (x: xs) set acc =
      let newAcc = acc + x in
      case newAcc `I.member` set of
        True -> newAcc
        _ -> go xs (newAcc `I.insert` set) newAcc

day1 :: IO ()
day1 = do
  i <- lines <$> readFile "data/day1.txt"
  let nums = catMaybes (parse <$> i)
  print $ sum nums
  print $ dup (cycle nums)
