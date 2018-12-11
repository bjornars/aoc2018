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

focus (x, y) = [(x+dx, y+dy) | dx <- [0,1,2], dy <- [0,1,2]]
shrink n ((a,b), (c,d)) = ((a,b), (c-n, d-n))

dayB = do
  let b = ((1,1), (300,300))
  let serial = 9810

  let arr = array b [uncurry (cell serial) <$> (xy, xy) | xy <- range b]

  let newBounds = shrink 2 b
  let sumArr = array newBounds [(xy, sum $ fmap (arr!) (focus xy)) | xy <-range newBounds]

  let biggest = maximumBy (compare `on` snd) (assocs sumArr)
  print biggest
