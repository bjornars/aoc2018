{-# LANGUAGE LambdaCase #-}

module DayC where

import Control.Arrow
import Control.Applicative ((<|>))
import Control.Monad
import Data.Maybe (catMaybes, fromJust)
import Data.List (elem)
import qualified Data.IntSet as S

import Debug.Trace
import Text.ParserCombinators.ReadP

(>:>) = flip $ (.) . (.)

type Plant = Bool
type Rule = [Plant]

plantsToSet plants = S.fromList . map fst . filter snd $ zip [0..] plants

parsePlant :: ReadP Plant
parsePlant = (char '#' *> pure True) <|> (char '.' *> pure False)

parseInit = do
  string "initial state: "
  plantsToSet <$> many parsePlant <* char '\n'

parseRule = do
  input <- replicateM 5 parsePlant
  string " => "
  output <- parsePlant
  char '\n'
  pure $ if output
    then Just input
    else Nothing

parser :: ReadP (S.IntSet, [[Plant]])
parser = do
  initState <- parseInit
  char '\n'
  rules <- many $ parseRule
  eof
  return (initState, catMaybes rules)

parse :: ReadP a -> String -> Maybe a
parse  = readP_to_S >:> \case
  [(res, "")] -> Just res
  _ -> Nothing

grow :: [Rule] -> S.IntSet -> S.IntSet
grow rules planter =
  let min = S.findMin planter
      max = S.findMax planter
      in foldr go S.empty [min-2..max+2]
    where go idx s = let
            foci = flip S.member planter <$> [idx-2 .. idx +2]
            shouldSpawn xs = elem xs rules
            in if shouldSpawn foci then S.insert idx s else s

dropUntilStable :: (Eq b) => (a -> b) -> [a] -> a
dropUntilStable key xs = go xs []
  where go (x:xs) history
          | length history == 10 && all (==key x) history = x
          | otherwise = go xs (key x:take 9 history)

dayC = do
  let ssum = sum . S.toList
  (init, rules) <- fromJust . parse parser <$> readFile "data/dayC.txt"
  putStr "Part 1: "
  print . ssum . (!!20) . iterate (grow rules) $ init

  -- find a somewhat stable generation, where everthing has degenerated
  -- into lliders
  let (gen, plants) =
        dropUntilStable (S.size . snd)
        . iterate ((+1) *** grow rules)
        $ (0::Integer, init)

  -- interpolate!
  let x1 = toInteger $ ssum plants
  let x2 = toInteger $ ssum $ grow rules plants
  let target = 50 * 1000 * 1000 * 1000
  putStr "Part 2: "
  print (x1 + (x2 - x1) * (target - gen))
