{-# LANGUAGE TemplateHaskell       #-}

module DayG where

import Data.Bits
import Data.Bool
import qualified Data.Map as M
import Data.Map ((!))
import Data.Maybe

import Control.Arrow
import Control.Monad.State
import Control.Lens
import Debug.Trace

import Lib
import Text.ParserCombinators.ReadP
import Text.Printf


type Registers = [Int]
type O a = State Registers a

load :: Int -> O Int
load addr = gets (^?! ix addr)

store :: Int -> Int -> O ()
store addr val = modify (set (ix addr) val)

flag :: Bool -> O Int
flag = pure . bool 0 1

data Op = Op { _name :: String,  _exec :: (Int, Int, Int) -> O () }
makeLenses ''Op
ops =
   [Op "addr" (\(x, y, c) -> (+) <$> load x <*> load y >>= store c)
  , Op "addi" (\(x, y, c) -> (+) <$> load x <*> pure y >>= store c)
  , Op "mulr" (\(x, y, c) -> (*) <$> load x <*> load y >>= store c)
  , Op "muli" (\(x, y, c) -> (*) <$> load x <*> pure y >>= store c)
  , Op "banr" (\(x, y, c) -> (.&.) <$> load x <*> load y >>= store c)
  , Op "bani" (\(x, y, c) -> (.&.) <$> load x <*> pure y >>= store c)
  , Op "borr" (\(x, y, c) -> (.|.) <$> load x <*> load y >>= store c)
  , Op "bori" (\(x, y, c) -> (.|.) <$> load x <*> pure y >>= store c)
  , Op "setr" (\(x, _, c) -> load x >>= store c)
  , Op "seti" (\(x, _, c) -> pure x >>= store c)
  , Op "gtir" (\(x, y, c) -> (>) <$> pure x <*> load y >>= flag >>= store c)
  , Op "gtri" (\(x, y, c) -> (>) <$> load x <*> pure y >>= flag >>= store c)
  , Op "gtrr" (\(x, y, c) -> (>) <$> load x <*> load y >>= flag >>= store c)
  , Op "eqir" (\(x, y, c) -> (==) <$> pure x <*> load y >>= flag >>= store c)
  , Op "eqri" (\(x, y, c) -> (==) <$> load x <*> pure y >>= flag >>= store c)
  , Op "eqrr" (\(x, y, c) -> (==) <$> load x <*> load y >>= flag >>= store c)
  ]

parseSpec str = do
  string str >> string ":" >> skipSpaces
  between (char '[') (string "]\n") $ sepBy1 digit (string ", ")

parseExpect = do
  before <- parseSpec "Before"
  [op, a, b, c] <- count 4 (digit <* skipSpaces)
  after <- parseSpec "After"
  string "\n"
  return (before, after, (a, b, c))

parseFile = do
  expects <- many parseExpect
  string "\n"
  string "\n"
  munch (const True)
  return expects

findEq :: Registers -> (Int, Int, Int) -> Registers -> [String]
findEq inputs args expected = let
  run args op = (op, execState (op ^. exec $ args) inputs)
  res = run args <$> ops
  in map (^._1.name) . filter ((==expected) . snd) $ res

dayG :: IO ()
dayG = do
  i <- readFile "data/dayG.txt"
  let expected = fromJust $ parse parseFile i
  let go (before, after, args) = findEq before args after
  let m = go <$> expected
  let matches = length <$> m

  printf "Part1: %d\n" . length . filter (>2) $ matches
