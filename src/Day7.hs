{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Day7 (day7) where

import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Char
import Data.Maybe
import Data.List
import Data.Tuple

import qualified Data.Map as M
import Text.ParserCombinators.ReadP hiding (get)
import qualified Text.ParserCombinators.ReadP as P

import Debug.Trace

type Dep = (Char, Char)

parser :: ReadP Dep
parser = string "Step " >> ((,)
  <$> (P.get <* string " must be finished before step ")
  <*> (P.get <* string " can begin." <* eof))

parse :: String -> Maybe Dep
parse = readP_to_S parser >>> \case
  [(d, "")] ->  Just d
  _ -> Nothing

data Workers = Workers {
  _numWorkers :: Int,
  _progress :: [(Char, Int)],
  _timeElapsed :: Int
  } deriving Show

makeClassy ''Workers

cost :: Char -> Int
cost c = ord c - ord 'A' + 1 + 60

topo :: M.Map Char [Char] -> State Workers [Char]
topo deps = do
  n <- use numWorkers
  tasks  <- use progress
  timeElapsed += 1
  let (done, tasks') = tick tasks
  let open = n - length tasks'

  unless (null done) $ do
    traceM $ "we are done with " <> show done
    traceM $ "and have " <> show n <> " open tasks"

  -- remove the newly done from the dependecy lists
  let deps' = fmap (filter (not.(`elem` done))) deps

  -- take new tasks to fill `open` number of spots
  let next = take open . sort . map fst . filter (null . snd) $ M.toList deps'
  let newTasks = tasks' <> ((id &&& cost) <$> next)
  progress .= newTasks

  unless (null next) $
    traceM $ "taking new tasks: " <> show next
  -- remove the newly taken from deps
  let deps'' = foldr M.delete deps' next

  if null newTasks then return done
  else (done ++) <$> topo deps''

  where tick :: [(Char, Int)] -> ([Char], [(Char, Int)])
        tick xs = let step = (fmap.second) (subtract 1) xs
                      (done, next) = partition ((== 0).snd) step
                  in (fst <$> done, next)

day7 :: IO ()
day7 = do
  deps <- fromJust . traverse parse . lines <$> readFile "data/day7.txt"
  -- let deps = [('C', 'A'), ('C', 'F'), ('A', 'B'),
  --             ('A', 'D'), ('B', 'E'), ('D', 'E'), ('F', 'E')]

  let steps = nub . sort $ (fst <$> deps) <> (snd <$> deps)
  let d = fmap asList . swap <$> deps where asList a = [a]
  -- make a map from step to its dependencies, and make sure to add in
  -- starting-points
  let m = M.fromListWith (++) d `M.union` M.fromList [(x, []) | x <- steps]
  let (res, st) = runState (topo m) (Workers 5 [] (-1))
  putStrLn $ "Part1: " <> res <> ", " <> show st
