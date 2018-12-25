{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import Control.Arrow
import Data.Char
import Data.Semigroup
import Data.Function (on)
import Data.List (groupBy, sortBy)
import Text.ParserCombinators.ReadP

(...) = (.) . (.)
(>:>) = flip $ (...)
infixr 8 ...

digit :: ReadP Int
digit = (read <$> many1 (satisfy isDigit)) <++
        (char '-' *> (negate <$> digit))

parse :: ReadP a -> String -> Maybe a
parse  = readP_to_S >:> \case
  [(res, "")] -> Just res
  _ -> Nothing

distance :: Num a => (a, a) -> (a, a) -> a
distance (x1, y1) (x2, y2) = abs (x2 - x1) + abs (y2 - y1)

distance3 :: Num a => (a, a, a) -> (a, a, a) -> a
distance3 (x1, y1, z1) (x2, y2, z2) = abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1)

distance4 :: Num a => (a, a, a, a) -> (a, a, a, a) -> a
distance4 (x1, y1, z1, w1) (x2, y2, z2, w2) = abs (x2 - x1) + abs (y2 - y1) + abs (z2 - z1) + abs (w2 - w1)

groupSort key = groupBy ((==) `on` key) . sortBy (compare `on` key)

minmax :: (Ord a, Num a, Bounded a) => [a] -> (a, a)
minmax = (getMin *** getMax) . foldMap (Min &&& Max)
