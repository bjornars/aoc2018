module DayB where

import Data.Array.Unboxed
import Data.Bifunctor
import Data.Function
import Data.Foldable
import Control.Arrow

hundreds :: Int -> Int
hundreds p = (-5 +) $ (p `mod` 1000) `div` 100
cell :: Int -> Int ->Int ->   Int
cell serial x y = let
  rack = x + 10
  p = rack * y
  p' = (p + serial) * rack
  in
  hundreds p'

focus n (x, y) = [(x+dx, y+dy) | dx <- [0..n-1], dy <- [0..n-1]]
shrink n ((a,b), (c,d)) = ((a,b), (c-n, d-n))

b = ((1,1), (300, 300))
mk_arr :: Int -> UArray (Int, Int) Int
mk_arr serial = array b [uncurry (cell serial) <$> (xy, xy) | xy <- range b]

dayB = do
  -- the matrix is heavily biased to negative numbers, so no need to go
  -- too high. I aso havent got all day.
  let arr = mk_arr 9810
  putStrLn $ "Part 1: " <> show (findBiggest arr 3)

  let maxima = (id &&& findBiggest arr) <$> [x | x <- [1..15]]
  let biggestest = (\(n, ((x, y), _)) -> (x, y, n)) $ maximumBy (compare `on` (snd.snd)) maxima


  putStrLn $ "Part 2: " <> show biggestest

findBiggest arr size =
  let newBounds = shrink size b
      sumArr = [(xy, sum $ fmap (arr!) (focus size xy)) | xy <-range newBounds]

  in maximumBy (compare `on` snd) (sumArr)
