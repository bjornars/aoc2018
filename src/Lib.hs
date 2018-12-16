{-# LANGUAGE LambdaCase #-}

module Lib where

import Data.Char
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Text.ParserCombinators.ReadP

(>:>) = flip $ (.) . (.)

digit :: ReadP Int
digit = read <$> many1 (satisfy isDigit)

parse :: ReadP a -> String -> Maybe a
parse  = readP_to_S >:> \case
  [(res, "")] -> Just res
  _ -> Nothing

distance :: Num a => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

groupSort key = groupBy ((==) `on` key) . sortBy (compare `on` key)
