{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module DayD where

import Control.Arrow
import Control.Monad
import Control.Lens
import Data.Array
import Data.List (findIndex, sortBy, transpose)
import Data.Function (on)
import Data.Maybe (catMaybes, fromJust)

import Debug.Trace

data Dir = L | S | R deriving Show
data Cart = Cart { _cix :: Dim
                 , _cchar ::  Char
                 , _cdir ::  [Dir] }

instance Show Cart where
  show (Cart ix char dir) = "Cart(" <> show ix <> ", " <> [char] <> ", " <> show (head dir) <> ")"

type Dim = (Int, Int)
type Arr = Array Dim Char

makeLenses ''Cart

type Track = [Dim]

turn x S = x
turn 'v' L = '>'
turn 'v' R = '<'
turn '^' L = '<'
turn '^' R = '>'
turn '<' L = 'v'
turn '<' R = '^'
turn '>' L = '^'
turn '>' R = 'v'

findCollision :: Arr -> [Cart] -> Dim
findCollision arr carts = traceShow carts $ go (sortBy (compare `on` _cix) carts) []
  where go [] [] = undefined
        go [] processed = findCollision arr processed
        go (next: rest) processed =
          let next' = processMove arr next
          in if collisions next' (rest <> processed)
                then _cix next'
                else go rest (processed <> [next'])

collisions x xs = let x' = _cix x in any ((==x')._cix) xs

inc :: Char -> (Int, Int) -> (Int, Int)
inc 'v' = second (+1)
inc '^' = second (subtract 1)
inc '>' = first (+1)
inc '<' = first (subtract 1)

step n@(Cart _ c _) = n & cix %~ inc c

curve '/'  n@(Cart _ '>' _) = n & cchar .~ '^' & cix %~ inc '^'
curve '/'  n@(Cart _ '<' _) = n & cchar .~ 'v' & cix %~ inc 'v'
curve '\\' n@(Cart _ '>' _) = n & cchar .~ 'v' & cix %~ inc 'v'
curve '\\' n@(Cart _ '<' _) = n & cchar .~ '^' & cix %~ inc '^'
curve '/'  n@(Cart _ '^' _) = n & cchar .~ '>' & cix %~ inc '>'
curve '/'  n@(Cart _ 'v' _) = n & cchar .~ '<' & cix %~ inc '<'
curve '\\' n@(Cart _ '^' _) = n & cchar .~ '<' & cix %~ inc '<'
curve '\\' n@(Cart _ 'v' _) = n & cchar .~ '>' & cix %~ inc '>'


processMove :: Arr -> Cart -> Cart
processMove arr next = case arr ! (_cix next) of
  '-' -> step next
  '|' -> step next
  '/' -> curve '/' next
  '\\' -> curve '\\' next
  '+' -> intersection next
  where intersection cart@(Cart ix' c (d:dir)) =
          let c' = turn c d in
              cart & cchar .~ c' & cix %~ inc c' & cdir .~ dir


cartToLine x | x `elem` "<>" = '-'
cartToLine x | x `elem` "^v" = '|'
cartToLine x = x

dayD = do
  input <- lines <$> readFile "data/dayD.txt"
--  let input = [
--                 "/->-\\        "
--                , "|   |  /----\\"
--                , "| /-+--+-\\  |"
--                , "| | |  | v  |"
--                , "\\-+-/  \\-+--/"
--                , "  \\------/   "   ]
  let dim = ((length . head $ input), length input)
  let arr = listArray ((1, 1), dim) . concat . transpose $ input
  let carts = (\(ix, c) -> Cart ix c (cycle [L, S, R])) <$>
                filter ((`elem` "<>v^").snd) (assocs arr)

  let arr' = cartToLine <$> arr
  let ix = findCollision arr' carts
  putStr "Part 1: "
  print $ ((subtract 1) *** (subtract 1)) ix
