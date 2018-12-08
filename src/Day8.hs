{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Day8 where

import Control.Monad.State
import Data.Monoid

import Text.Read hiding (get)

data Node a = Node [Node a] [a] deriving (Show)

instance Functor Node where
  fmap f (Node ns xs) = Node (fmap f <$> ns) (fmap f xs)

instance Foldable Node where
  foldMap f (Node ns xs) = mconcat (foldMap f <$> ns) <> foldMap f xs


value (Node [] xs) = sum xs
value (Node ns xs) = sum (value <$> nodes)
  where count xs a = length $ filter (==a) xs
        nodes = concat $ zipWith replicate (count xs <$> [1..]) ns

type Parser i o = State [i] o


eat :: Int -> Parser i [i]
eat n = do
  input <- get
  let (xs, rest) = splitAt n input
  put rest
  return xs

one :: Parser i i
one = head <$> eat 1

parseNode :: Parser Int (Node Int)
parseNode = do
  n_children <- one
  n_meta <- one
  children <- replicateM n_children parseNode
  metadata <- eat n_meta
  return $ Node children metadata


runParser :: Parser Int (Node Int) -> [Int] -> Maybe (Node Int)
runParser parser input =
  let (result, remainingState) = runState parser input
  in
  if null remainingState then Just result
  else Nothing

day8 = do
  input <- traverse (readMaybe @Int) . words <$> readFile "data/day8.txt"
  let tree = input >>= runParser parseNode
  case tree of
    Just n -> do
      putStrLn $ "Part 1: " <> show (sum n)
      putStrLn $ "Part 2: " <> show (value n)
    Nothing -> putStrLn "Parse error, somewhere"
