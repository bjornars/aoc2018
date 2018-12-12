{-# LANGUAGE LambdaCase #-}

module DayC where

import Control.Applicative ((<|>))
import Control.Monad
import Data.Bifunctor
import Data.Maybe (catMaybes, fromJust)
import Data.List (elem)
import qualified Data.Set as S

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

parser :: ReadP (S.Set Int, [[Plant]])
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

grow :: [Rule] -> S.Set Int -> S.Set Int
grow rules planter =
  let min = fromJust $ S.lookupMin planter
      max = fromJust $ S.lookupMax planter
      in foldr go S.empty [min-2..max+2]
    where go idx s = let
            foci = flip S.member planter <$> [idx-2 .. idx +2]
            shouldSpawn xs = elem xs rules
            in if shouldSpawn foci then S.insert idx s else s

dayC = do
  (init, rules) <- fromJust . parse parser <$> readFile "data/dayC.txt"
  print . sum . (!!20) . iterate (grow rules) $ init
