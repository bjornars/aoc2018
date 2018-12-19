{-# OPTIONS_GHC -Wall #-}

module DayB where

import Data.Array.Unboxed
import Data.Function
import Data.List
import Data.Foldable
import Control.Arrow ((&&&))

hundreds :: Int -> Int
hundreds p = (-5 +) $ (p `mod` 1000) `div` 100
cell :: Int -> Int ->Int ->   Int
cell serial x y = let
  rack = x + 10
  p = rack * y
  p' = (p + serial) * rack
  in
  hundreds p'

shrink :: Num b => b -> (b, b) -> (b, b)
shrink n (c,d) = (c-n, d-n)

nn :: Int
nn = 300

mk_arr :: Int -> UArray (Int, Int) Int
mk_arr serial = let
  -- make a NxN matrix of our cells
  seed = [ [cell serial x y | x <- [1..nn]] | y <- [1..nn] ]
  -- make a NxN matrix of the sum of all cells from (0, 0) to given point.
  cumsums = scanl (+) 0 <$> seed
  cumcumsumsums =  scanl (+) 0 <$> transpose cumsums
  in listArray ((0,0), (nn, nn)) $ concat cumcumsumsums

get_sq_sum :: UArray (Int, Int) Int
  -> Int -- box size
  -> (Int, Int) -- top left corner
  -> Int
get_sq_sum arr n (x, y) = sum [
  arr!(x+n, y+n), -- geometry!
  negate $ arr!(x+n,y),
  negate $ arr!(x,y+n),
  arr!(x,y)
  ]

dayB :: IO ()
dayB = do
  let arr = mk_arr 9810
  putStrLn $ "Part 1: " <> show (findBiggest arr 3)

  let maxima = (id &&& findBiggest arr) <$> [x | x <- [1..nn - 1]]
  traverse_ print maxima
  let biggestest = (\(n, ((x, y), _)) -> (x, y, n))
                   $ maximumBy (compare `on` (snd.snd)) maxima

  putStrLn $ "Part 2: " <> show biggestest

findBiggest :: UArray (Int, Int) Int
  -> Int -- box size
  -> ((Int, Int), Int)
findBiggest arr size =
  let search = ((1,1), shrink size (snd $ bounds arr))
      sumArr = [((x+1, y+1), get_sq_sum arr size (x, y))
               | (x, y) <-range search]

  in maximumBy (compare `on` snd) (sumArr)
