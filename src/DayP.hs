{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wall   #-}

module DayP where

import Data.Foldable (foldl', toList)
import Data.List     ((\\))
import Data.Maybe    (fromJust)
import Data.Set      (Set, singleton, union)

import Lib
import Text.ParserCombinators.ReadP

type Point = (Int, Int, Int, Int)

line :: ReadP Point
line = sepBy digit (char ',') >>= \case
  [x,y,z,w] -> pure (x,y,z,w)
  _ -> pfail

dayP :: IO ()
dayP = do
  i <- lines <$> readFile "data/dayP.txt"
  let points = fromJust . fmap (fmap singleton) <$> traverse (parse line) $ i
  print . length $ constellate points
  pure ()

constellate :: [Set Point] -> [Set Point]
constellate points = go points [] False
  where
  isClose xs zs = or [distance4 x z <= 3 | x <- toList xs, z <- toList zs]
  go [] zs True = go zs [] False
  go [] zs False = zs
  go (x:xs) zs found = let
    close = filter (isClose x) $ xs <> zs
    merged = foldl' union x close
    in if null close
      then go xs (x:zs) found
      else go (xs \\ close) (merged : (zs \\ close)) True
