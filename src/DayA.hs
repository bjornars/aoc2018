{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

module DayA where

import Control.Arrow
import Data.Array
import Data.Bool
import Data.Char
import Data.Foldable
import Data.Function
import Data.Maybe
import Data.List
import Data.Semigroup
import Data.Tuple
import Text.ParserCombinators.ReadP
import Debug.Trace

digit :: ReadP Int
digit = do
  neg <- char '-' <++ return ' '
  digits <- munch1 isDigit
  return $ read @Int $ neg:digits

pair :: String -> ReadP (Int, Int)
pair header = string header >> (between
                (char '<' >> skipSpaces)
                (char '>')
                ((,) <$> digit <* string "," <* skipSpaces <*> digit))

pair' = (,) <$> digit <* string ", " <*> digit

parser = do
  (x, y) <- pair "position=" <* skipSpaces
  (dx, dy) <- pair "velocity=" <* eof
  return ((x, y), ((+dx) *** (+dy)))

parse :: String -> Maybe _
parse = readP_to_S parser >>> \case
  [(res, "")] -> Just res
  _ -> Nothing

minmax :: [Int] -> (Int, Int)
minmax = (getMin *** getMax) . foldMap (Min &&& Max)

boundingBox :: [(Int, Int)] -> ((Int, Int), (Int, Int))
boundingBox points =
  let (x1, x2) = minmax $ fst <$> points
      (y1, y2) = minmax $ snd <$> points
  in ((x1, y1), (x2, y2))

bsize :: Num a => ((a, a), (a, a)) -> a
bsize ((x1, y1), (x2, y2)) = (x2 - x1) * (y2 - y1)

localMinima :: (Num b, Ord b) => (a -> b) -> [a] -> [a]
localMinima by xs@(x:_) = go xs (by x + 1) []
  where
  go [] _ prev = undefined
  go (x:xs) acc prev = let bx = by x in
    if bx < acc
      then go xs bx (x:take 1 prev)
      else x:prev

printSolution (gen, points) =
  let ((x1, y1), (x2, y2)) = boundingBox points
      translated = ((subtract x1) *** (subtract y1)) <$> points
      arr = listArray ((0, 0), (y2-y1, x2-x1)) $ repeat ' '
  in do
    putStrLn ("Gen #" <> show gen)
    printArr $ arr // zip (swap <$> translated) (repeat '\x2588')

printArr :: Array (Int, Int) Char -> IO ()
printArr arr =
  let rows = (map.map) (snd ) $ groupBy ((==) `on` (fst.fst)) $ assocs arr
  in putStr $ unlines rows

dayA :: IO ()
dayA = do
  input <- lines <$> readFile "data/dayA.txt"
  let parsed = fromJust . traverse parse $ input
  let (positions, velocities) = (fst <$> parsed, snd <$> parsed)
  let it = (+1) *** zipWith ($) velocities
  print $ bsize $ boundingBox positions
  let smallest = localMinima (bsize.boundingBox.snd) $ iterate it (0, positions)
  let extended = reverse smallest <> take 2 (iterate it (head smallest))
  putStrLn "Part1: "
  traverse_ printSolution extended
