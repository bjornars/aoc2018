{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall #-}

module DayK where

import qualified Data.Map            as M
import           Data.Maybe
import           Data.Sequence       hiding (filter, length)

import           Control.Monad.State

import           Text.Printf


type Coords = (Int, Int)


type Doors = M.Map (Int, Int) [(Int, Int)]
parse :: [Coords] -> Coords -> String -> State Doors ()
parse prev c@(x, y) (s:str) = do
  case s of
    'N' -> connect c (x, y-1) >>= \new -> parse prev new str
    'E' -> connect c (x+1, y) >>= \new -> parse prev new str
    'W' -> connect c (x-1, y) >>= \new -> parse prev new str
    'S' -> connect c (x, y+1) >>= \new -> parse prev new str
    '(' -> parse (c:prev) c str
    ')' -> parse (tail prev) c str
    '|' -> parse prev (head prev) str
    '$' -> pure ()
    _   -> parse prev c str


connect :: Coords -> Coords -> State Doors Coords
connect a b = do
  modify $ M.insertWith mappend a [b]
  modify $ M.insertWith mappend b [a]
  pure b


bfs :: Seq (Coords, Int) -> M.Map Coords Int -> Doors -> [Int]
bfs Empty visited _ = M.elems visited
bfs ((coords, len) :<| queue) visited cnxs
  | coords `M.member` visited = bfs queue visited cnxs
  | otherwise = let
      next = fromMaybe [] $ coords `M.lookup` cnxs
      next' = fromList $ (,len+1) <$> next
    in bfs (queue >< next') (M.insert coords len visited) cnxs


dayK :: IO ()
dayK = do
  i <- readFile "data/dayK.txt"
  let p = flip execState M.empty $ parse [] (0,0) i
  let paths = bfs (singleton ((0,0), 0)) M.empty p
  printf "Part 1: %d\n" (maximum paths)
  printf "Part 2: %d\n" (length . filter (>= 1000) $ paths)
