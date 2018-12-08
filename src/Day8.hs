{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeApplications #-}

module Day8 where

import Control.Monad.State
import Data.Monoid

import Text.Read hiding (lift, get)

data Node a = Node [Node a] [a] deriving (Show)

instance Functor Node where
  fmap f (Node ns xs) = Node (fmap f <$> ns) (fmap f xs)

instance Foldable Node where
  foldMap f (Node ns xs) = mconcat (foldMap f <$> ns) <> foldMap f xs


value (Node [] xs) = sum xs
value (Node ns xs) = sum (value <$> nodes)
  where count xs a = length $ filter (==a) xs
        nodes = concat $ zipWith replicate (count xs <$> [1..]) ns

type ParseError = String
type Parser i o = StateT [i] (Either ParseError) o


err :: String -> Parser a b
err str = lift $ Left str

eat :: Int -> Parser i [i]
eat n = do
  input <- get
  when (n > length input) $ err "ran out of input"
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


runParser :: Parser Int (Node Int) -> [Int] -> Either ParseError (Node Int)
runParser parser input = do
  (result, remainingState) <- runStateT parser input
  if null remainingState then Right result
  else Left "too much input"

note :: a -> Maybe b -> Either a b
note msg = maybe (Left msg) Right

day8 = do
  input <- note "couldnt parse file" . traverse (readMaybe @Int) . words <$> readFile "data/day8.txt"
  let tree = input >>= runParser parseNode
  case tree of
    Right n -> do
      putStrLn $ "Part 1: " <> show (sum n)
      putStrLn $ "Part 2: " <> show (value n)
    Left err -> putStrLn err
