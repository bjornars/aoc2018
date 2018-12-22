{-# OPTIONS -Wall #-}
{-# LANGUAGE TupleSections    #-}
{-# LANGUAGE TypeApplications #-}

module DayM where

import           Data.Array
import           Data.Function
import           Data.Ix       (inRange)
import           Data.Maybe

import qualified Data.Map      as M
import qualified Data.Set      as S
import           Text.Printf

import           Prelude       hiding (lookup)

data CType = Rocky | Wet | Narrow deriving (Show, Enum)

loeb :: Functor f => f (f b -> b) -> f b
loeb fs  = go where go = fmap ($go) fs

type C = (Int, Int)
toIdx :: C -> Int -> C -> (Array C Int -> Int)
toIdx target _ xy
  | xy == target = const 0
toIdx _ _ (0, 0) = const 0
toIdx _ _ (x, 0) = const $ 16807 * x
toIdx _ _ (0, y) = const $ 48271 * y
toIdx _ depth (x, y) = let
  (*%) = ((*) `on` (`mod` 20183) . (+depth))
  in \arr -> (arr!(x,y-1)) *% (arr!(x-1, y))

-- showT Rocky  = '1'
-- showT Wet    = '2'
-- showT Narrow = '4'
--
-- pprint arr = let ((x1, y1), (x2, y2)) = bounds arr in
--     intercalate "\n" $ [
--       map showT [arr!(x, y) | x <- [x1..x2]]
--       | y <- [y1..y2]]


data Tool = None | Light | Hook deriving (Eq, Enum, Ord, Show)
ok :: Tool -> CType -> Bool
ok Light Wet   = False
ok Hook Narrow = False
ok None Rocky  = False
ok _ _         = True

calcMap :: (C, C) -> Int -> C -> Array C CType
calcMap b depth target = let
  -- let (depth, target) = (510, (10,10))
  gis = loeb $ listArray b (toIdx target depth  <$> range b)
  in (toEnum @CType . (`mod` 3) . (`mod` 20183) .  (+depth)) <$> gis

dayM :: IO ()
dayM = do
  let (depth, target) = (5913, (8,701))
  let el = calcMap ((0, 0), target) depth target
  printf "Part1: %d\n" (sum . fmap fromEnum $ el)

  let el' = calcMap ((0, 0), (100, 1000)) depth target
  let lookup c = if inRange (bounds el') c then Just (el' ! c) else Nothing
  let start = S.singleton (0, (0,0), Light, [])
  let paths = bfs start M.empty lookup
  let path = fromMaybe [] $ (target, Light) `M.lookup` paths
  let (cost, _, _) = head path
  printf "Part1: %d\n" (cost)

adjacents :: C -> [C]
adjacents (x, y) = [(x, y-1),(x-1, y),(x+1, y),(x, y+1)]

type Path = ([(Int, Tool, C)])
bfs :: S.Set (Int, C, Tool, Path) -- queue
    -> M.Map (C, Tool) Path       -- visited
    -> (C -> Maybe CType)         -- map over terrain types
    -> M.Map (C, Tool) Path        --map over shortest path to coords with tools

bfs q visited lookup = case S.minView q of
    Nothing -> visited
    Just ((len, coords, tool, path), q')
      | (coords, tool) `M.member` visited -> bfs q' visited lookup
      | otherwise -> let
          options = [
            (len + cost, c, t, (len + cost, t, c):path)
            | t <-[None, Light, Hook]
            , c <- adjacents coords
            , let cost = if t == tool then 1 else 8
            , ok t $ fromJust (lookup coords)
            , fromMaybe False $ (ok t) <$> lookup c]

          next = q' <>  (S.fromList options)
        in bfs next (M.insert (coords, tool) ((len, tool, coords):path) visited) lookup
