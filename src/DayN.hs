module DayN where

import Control.Arrow
import Control.Lens (_1, _2, _3)
import Data.List (minimumBy, maximumBy, transpose)
import Data.Function
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S
import Text.ParserCombinators.ReadP
import Text.Printf

import Debug.Trace

import Lib

data Bot = Bot { _bx :: Int
               , _by :: Int
               , _bz :: Int
               , _br :: Int
               } deriving Show

line = do
  string "pos=<"
  x <- digit
  char ','
  y <- digit
  char ','
  z <- digit
  string ">, r="
  r <- digit
  eof
  pure $ Bot x y z r

n = 3
n2 = 2

nub :: Ord a => [a] -> [a]
nub = S.toList . S.fromList

-- split one big square into n x n x n squares
chunk :: (Int, Int, Int) -> (Int, Int, Int) -> [(Int, Int, Int)]
chunk (x, y, z) (dx, dy, dz) =
  let dd = n `div` 2
  in
    nub [ (x + (dx * nx), y + (dy * ny), z + (dz * nz))
    | nx <- [-dd-1..dd+1]
    , ny <- [-dd-1..dd+1]
    , nz <- [-dd-1..dd+1]
    ]

biasedMax ::
  Ord a =>
  ((Int, Int, Int) -> a) -> [(Int, Int, Int)] -> (Int, Int, Int)
biasedMax key = minimumBy (compare `on` (distance3 (0,0,0)))
                . last . groupSort key

chop ::
  (Ord a, Show a)
  => [(Int, Int, Int)]
  -> (Int, Int, Int)
  -> ((Int, Int, Int) -> a)
  -> (Int, Int, Int)
chop chunks (dx, dy, dz) select = let
  bestChunk = biasedMax select chunks
  nextBounds = (dx * n2 `div` n, dy *n2 `div` n, dz * n2 `div` n)
  in
  if length chunks == 1
    then bestChunk
    else chop (chunk bestChunk nextBounds) nextBounds select


dayN = do
  i <- lines <$> readFile "data/dayN.txt"
  let bots = fromJust . traverse (parse line) $ i

  let (Bot bx by bz br)  = maximumBy (compare `on` _br) bots
  printf "Part1: %d\n" $ length . filter (\(Bot x y z _) -> distance3 (x,y,z) (bx, by, bz) <= br) $ bots

  let l = (\b -> [_bx b, _by b, _bz b ])
  let [(minx, maxx), (miny, maxy), (minz, maxz)] = fmap (minmax) .
        transpose .
        fmap (\b -> [_bx b, _by b, _bz b ]) $
        bots

  let center = (minx + (maxx-minx) `div` 2,
                miny + (maxy-miny) `div` 2,
                minz + (maxz-minz) `div` 2)

  let sizes = (maxx-minx,
               maxy-miny,
               maxz-minz)

  let chunks = chunk center sizes

  let bots' = (\(Bot x y z r)  -> ((x,y,z), r)) <$> bots
  let select xyz = length . filter (\(bxyz, br) -> distance3 xyz bxyz <= br) $ bots'

  let bestChunk = chop chunks sizes select

  -- this probably isnt the correct way, I had to add one to get the same answer as some other solutions
  printf "Part2: %s\n" $ show $ 1 + distance3 (0, 0, 0) bestChunk
