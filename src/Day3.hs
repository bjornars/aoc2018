module Day3 where

import Data.Array
import Data.Array.ST

import Control.Monad (forM_, when)
import Data.Maybe (fromMaybe, isJust)
import Data.Monoid (Sum(..))
import Data.List (intercalate, elemIndex)
import Text.Read (readMaybe)

(...) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(...) = (.) . (.)
-- a ... b =  \x y -> a (b x y)

type Claim = (Int, Int, Int, Int)
type IX = (Int, Int)

claimToList :: (Int, Int, Int, Int) -> [IX]
claimToList (x, y, dx, dy) = range ((x, y), (x+dx-1, y+dy-1))

parse :: String -> Maybe Claim
parse line = case words line of
    [_, _, start, size] -> parseCoords (init start) size
    _ -> Nothing

splitOn :: Char -> String -> Maybe (String, String)
splitOn c str = (\idx -> (take idx str, drop (idx + 1) str))
                <$> elemIndex c str

parseCoords :: String -> String -> Maybe Claim
parseCoords start size = do
  (x, y) <- splitOn ',' start
  (dx, dy) <- splitOn 'x' size
  let r = readMaybe :: String -> Maybe Int
  (,,,) <$> r x <*> r y <*> r dx <*> r dy

fillArray :: (IX, IX) -> [Claim] -> Array IX (Sum Int)
fillArray ix cs = runSTArray $ do
  let indicies = cs >>= claimToList
  arr <- newArray ix 0
  indicies `forM_` \e -> do
    x <- readArray arr e
    writeArray arr e (x + 1)
  return arr

isSingle arr claim = all (==1) $ (arr !) <$> claimToList claim

day3 :: IO ()
day3 = do
  inputs <- lines <$> readFile "data/day3.txt"
  let parsed = traverse parse inputs
  case parsed of
    Nothing -> print "no parse"
    Just c -> do
      let maxX = maximum $ (\(x, _, dx, _) -> x + dx) <$> c
      let maxY = maximum $ (\(_, y, _, dy) -> y + dy) <$> c
      let filled = fillArray ((0, 0), (maxX, maxY)) c
      print $ length $ filter ((>=2) . getSum) $ elems filled
      let singleClaims = snd <$> filter (isSingle filled . fst) (zip c [1..])
      putStrLn $ "Proper claims: "
        <> intercalate "," (("#" <>) . show <$> singleClaims)
