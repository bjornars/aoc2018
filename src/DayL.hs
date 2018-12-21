{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall              #-}

module DayL where

import           Data.Bits
import qualified Data.IntSet                     as S

import           Text.Printf

key = 1107552

dayL :: IO ()
dayL = do
  -- i <- readFile "data/dayL.txt"
  let (part1, part2) = gogo
  printf "Part1: %d\n" part1
  printf "Part2: %d\n" part2

gogo :: (Int, Int)
gogo = stepProgram key 65536 S.empty S.empty 0 0


-- result of disassembling elfscript
stepProgram :: Int -> Int -> S.IntSet -> S.IntSet -> Int -> Int -> (Int, Int)
stepProgram c d cs ds smallest biggest = let
  n = 16777216
  e = d  `mod` 256
  c' = c + e
  c'' = ((c' `mod` n) * 65899) `mod` n
  d' = c'' .|. 65536
  biggest' = if c'' `S.member` cs then biggest else c''
  in if d >= 256
    then stepProgram c'' (d `div` 256) cs ds smallest biggest
    else
      if S.null cs
        then stepProgram key  d' (S.singleton c'') (d' `S.insert` ds) c'' biggest'
      else if d' `S.member` ds
        then (smallest, biggest')
        else stepProgram key  d' (c'' `S.insert` cs) (d' `S.insert` ds) smallest biggest'
