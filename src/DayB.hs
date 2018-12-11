module DayB where

import Data.Array.Unboxed
import Data.Bifunctor
import Data.Function
import Data.Foldable

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

dayB = do
  let serial = 9810
  -- the matrix is heavily biased to negative numbers, so no need to go
  -- too high. I aso havent got all day.
  let maxima = (second $ findBiggest serial) <$> [(x, x) | x <- [1..30]]
  mapM_ print maxima
  print $ (\(n, ((x, y), _)) -> (x, y, n)) $ maximumBy (compare `on` (snd.snd)) maxima

findBiggest serial size =
  let b = ((1,1), (300, 300))
      arr :: UArray (Int, Int) Int
      arr = array b [uncurry (cell serial) <$> (xy, xy) | xy <- range b]
      newBounds = shrink size b
      sumArr = [(xy, sum $ fmap (arr!) (focus size xy)) | xy <-range newBounds]

  in maximumBy (compare `on` snd) (sumArr)
