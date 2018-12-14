{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Day8 where

import Control.Arrow
import Control.Monad
import Debug.Trace
import Data.Char

import Text.ParserCombinators.ReadP

data Node a = Node [Node a] [a] deriving (Show)

instance Functor Node where
  fmap f (Node ns xs) = Node (fmap f <$> ns) (fmap f xs)

instance Foldable Node where
  foldMap f (Node ns xs) = mconcat (foldMap f <$> ns) <> foldMap f xs


value (Node [] xs) = sum xs
value (Node ns xs) = sum (value <$> nodes)
  where count xs a = length $ filter (==a) xs
        nodes = concat $ zipWith replicate (count xs <$> [1..]) ns

parseNode :: ReadP (Node Int)
parseNode = do
   n_children <- parseDigit
   n_meta <- parseDigit
   Node
     <$> replicateM n_children parseNode
     <*> replicateM n_meta parseDigit
    where parseDigit = char ' ' >> read @Int <$> many1 (satisfy isDigit)

runParser :: ReadP (Node Int) -> String -> Either String (Node Int)
runParser parser = readP_to_S (parser) >>> \case
  [(res, _)] -> Right res
  [] -> Left "no parse"

day8 = do
  input <- readFile "data/day8.txt"
  let tree = runParser parseNode (' ': input)
  case tree of
    Right n -> do
      putStrLn $ "Part 1: " <> show (sum n)
      putStrLn $ "Part 2: " <> show (value n)
    Left err -> putStrLn err
