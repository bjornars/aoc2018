module Day2 where

import Data.List
import Data.Monoid

checksums :: String -> (Sum Int, Sum Int)
checksums f =
  let counts = map length . group . sort $ f
      has = Sum . fromEnum . (`elem` counts)
  in (has 2, has 3)

day2 :: IO ()
day2 = do
  i <- lines <$> readFile "data/day2.txt"
  let (Sum doubles, Sum triples) = mconcat $ checksums <$> i
  print (doubles * triples)
