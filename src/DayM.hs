{-# OPTIONS -Wall #-}
{-# LANGUAGE TypeApplications #-}

module DayM where

import           Data.Array
import           Data.Function
import           Text.Printf

data CType = Rocky | Wet | Narrow deriving (Show, Enum)

loeb :: Functor f => f (f b -> b) -> f b
loeb fs  = go where go = fmap ($go) fs

type C = (Int, Int)
toIdx :: C -> Int -> C -> (Array C Int -> Int)
toIdx target _ xy
  | xy == target = const 0
toIdx _ _ (0, 0) = const 0
toIdx _ _ (x, 0) = const $ 16807 * x
toIdx _ _ (0, y) = const $ 48271 * y
toIdx _ depth (x, y) = let
  (*%) = ((*) `on` (`mod` 20183) . (+depth))
  in \arr -> (arr!(x,y-1)) *% (arr!(x-1, y))

-- showT Rocky = '.'
-- showT Wet = '='
-- showT Narrow = '|'
--
-- pprint arr = let ((x1, y1), (x2, y2)) = bounds arr in
--     intercalate "\n" $ [
--       map showT [arr!(x, y) | x <- [x1..x2]]
--       | y <- [y1..y2]]

dayM :: IO ()
dayM = do
  let (depth, target) = (5913, (8,701))
  let b = ((0, 0), target)
  let gis = loeb $ listArray b (toIdx target depth  <$> range b)
  let el = (toEnum @CType . (`mod` 3) . (`mod` 20183) .  (+depth)) <$> gis
  printf "Part1: %d\n" (sum . fmap fromEnum $ el)
