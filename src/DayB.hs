module DayB where

import Data.Array
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
  print $ findBiggest serial 3

findBiggest serial size =
  let b = ((1,1), (300, 300))
      arr = array b [uncurry (cell serial) <$> (xy, xy) | xy <- range b]
      newBounds = shrink size b
      sumArr = array newBounds [(xy, sum $ fmap (arr!) (focus size xy)) | xy <-range newBounds]

  in maximumBy (compare `on` snd) (assocs sumArr)
