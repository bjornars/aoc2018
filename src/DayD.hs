{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}

module DayD where

import Control.Arrow
import Control.Monad
import Control.Lens
import Data.Array
import Data.List (find, findIndex, sortBy, transpose)
import Data.Function (on)
import Data.Maybe (catMaybes, fromJust, isJust)

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

turn x   S = x
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
  where go :: [Cart] -> [Cart] -> Dim
        go [] [] = undefined
        go [] processed = findCollision arr processed
        go [survivor] [] = traceShow " one left " $  _cix survivor
        go (next: rest) processed =
          let next' = processMove arr next
              colliders = collisions next (rest <> processed)
          in if (null colliders)
                then go rest (processed <> [next'])
                else let strip xs = [x | x <- xs, isJust $ find ((/=_cix x)._cix) colliders]
                         in (go `on` strip) rest processed

collisions :: Cart -> [Cart] -> [Cart]
collisions x xs = let x' = _cix x in filter ((==x')._cix) xs

inc :: Char -> (Int, Int) -> (Int, Int)
inc 'v' = second (+1)
inc '^' = second (subtract 1)
inc '>' = first (+1)
inc '<' = first (subtract 1)

run :: Cart -> Char -> Cart
run n c = n & cchar .~ c & cix %~ inc c

curve track n@(Cart _ dir _) = run n . fromJust $ lookup (dir, track) turns
  where
    turns = [ (('>', '/' ), '^')
            , (('<', '/' ), 'v')
            , (('>', '\\'), 'v')
            , (('<', '\\'), '^')
            , (('^', '/' ), '>')
            , (('v', '/' ), '<')
            , (('^', '\\'), '<')
            , (('v', '\\'), '>')]


processMove :: Arr -> Cart -> Cart
processMove arr next = case arr ! (_cix next) of
  '-' -> step next
  '|' -> step next
  '/' -> curve '/' next
  '\\' -> curve '\\' next
  '+' -> intersection next
  where step n@(Cart _ c _) = run n c
        intersection cart@(Cart ix' c (d:dir)) =
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
  putStr "Part 2: "
  print $ ((subtract 1) *** (subtract 1)) ix
