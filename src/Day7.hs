{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# OPTIONS_GHC -Wall #-}

module Day7 (day7) where

import Control.Arrow
import Control.Monad.State
import Data.Maybe
import Data.List
import Data.Tuple

import qualified Data.Map as M
import Text.ParserCombinators.ReadP hiding (get)
import qualified Text.ParserCombinators.ReadP as P

type Dep = (Char, Char)

parser :: ReadP Dep
parser = string "Step " >> ((,)
  <$> (P.get <* string " must be finished before step ")
  <*> (P.get <* string " can begin." <* eof))

parse :: String -> Maybe Dep
parse = readP_to_S parser >>> \case
  [(d, "")] ->  Just d
  _ -> Nothing

topo :: M.Map Char [Char] -> State () [Char]
topo deps | M.null deps = pure []
topo deps =
  -- find the smallest key wth no values
  let next = minimum . map fst . filter (null . snd) $ M.toList deps
  in
  -- return the smallest, and remove this step from the map
  (next :) <$> (topo $ fmap (delete next) $ M.delete next deps)

day7 :: IO ()
day7 = do
  deps <- fromJust . traverse parse . lines <$> readFile "data/day7.txt"
  let steps = nub . sort $ (fst <$> deps) <> (snd <$> deps)
  let d = fmap asList . swap <$> deps where asList a = [a]
  -- make a map from step to its dependencies, and make sure to add in
  -- starting-points
  let m = M.fromListWith (++) d `M.union` M.fromList [(x, []) | x <- steps]
  let (res, st) = runState (topo m) ()
  putStrLn $ "Part1: " <> res <> ", " <> show st
