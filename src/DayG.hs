{-# LANGUAGE TemplateHaskell       #-}
{-# OPTIONS_GHC -Wall              #-}

module DayG where

import Data.Bits
import Data.Bool
import Data.Maybe
import Data.List

import Control.Monad.State
import Control.Lens

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

data Op = Op { _name :: String
             , _opcode :: Maybe Int
             , _exec :: (Int, Int, Int) -> O () }

instance Show Op where
  show (Op name code _) = "Op(" <> show name <> ", " <> show (fromMaybe (-1) code) <> ")" 
makeLenses ''Op

ops :: [Op]
ops =
   [Op "addr" Nothing (\(x, y, c) -> (+) <$> load x <*> load y >>= store c)
  , Op "addi" Nothing (\(x, y, c) -> (+) <$> load x <*> pure y >>= store c)
  , Op "mulr" Nothing (\(x, y, c) -> (*) <$> load x <*> load y >>= store c)
  , Op "muli" Nothing (\(x, y, c) -> (*) <$> load x <*> pure y >>= store c)
  , Op "banr" Nothing (\(x, y, c) -> (.&.) <$> load x <*> load y >>= store c)
  , Op "bani" Nothing (\(x, y, c) -> (.&.) <$> load x <*> pure y >>= store c)
  , Op "borr" Nothing (\(x, y, c) -> (.|.) <$> load x <*> load y >>= store c)
  , Op "bori" Nothing (\(x, y, c) -> (.|.) <$> load x <*> pure y >>= store c)
  , Op "setr" Nothing (\(x, _, c) -> load x >>= store c)
  , Op "seti" Nothing (\(x, _, c) -> pure x >>= store c)
  , Op "gtir" Nothing (\(x, y, c) -> (>) <$> pure x <*> load y >>= flag >>= store c)
  , Op "gtri" Nothing (\(x, y, c) -> (>) <$> load x <*> pure y >>= flag >>= store c)
  , Op "gtrr" Nothing (\(x, y, c) -> (>) <$> load x <*> load y >>= flag >>= store c)
  , Op "eqir" Nothing (\(x, y, c) -> (==) <$> pure x <*> load y >>= flag >>= store c)
  , Op "eqri" Nothing (\(x, y, c) -> (==) <$> load x <*> pure y >>= flag >>= store c)
  , Op "eqrr" Nothing (\(x, y, c) -> (==) <$> load x <*> load y >>= flag >>= store c)
  ]

type Case = ([Int], [Int], Int, (Int, Int, Int))
type Instruction = (Int, Int, Int, Int)


parseSpec :: String -> ReadP [Int]
parseSpec str = do
  string str >> string ":" >> skipSpaces
  between (char '[') (string "]\n") $ sepBy1 digit (string ", ")

parseExpect :: ReadP Case
parseExpect = do
  before <- parseSpec "Before"
  [op, a, b, c] <- count 4 (digit <* skipSpaces)
  after <- parseSpec "After"
  string "\n"
  return (before, after, op, (a, b, c))

parseRow :: ReadP (Int, Int, Int, Int)
parseRow = do
  skipSpaces
  a <- digit <* char ' '
  b <- digit <* char ' '
  c <- digit <* char ' '
  d <- digit <* char '\n'
  return (a,b,c,d)

parseFile :: ReadP ([Case], [Instruction])
parseFile = do
  expects <- many parseExpect
  string "\n\n"
  program <- many1 parseRow
  eof
  return (expects, program)


findEq :: Registers -> (Int, Int, Int) -> Registers -> [String]
findEq inputs args expected = let
  run op = (op, execState (op^.exec $ args) inputs)
  res = run <$> ops
  in map (^._1.name) . filter ((==expected) . snd) $ res

findUnique :: Case -> [Op] -> Maybe Op
findUnique (inputs, expected, opc, args) ops = let
  run op = (op, execState (op^.exec $ args) inputs)
  res = run <$> filter (has (opcode._Nothing)) ops
  in
    case filter ((==expected) . snd) res of
      [(op, _)] -> Just (op & opcode ?~ opc)
      _ -> Nothing


findOp :: [Case] -> [Case] -> [Op] -> Bool -> [Op]
findOp [] done ops True = findOp done [] ops False
findOp [] done ops False = ops
findOp (c:cs) done ops found = let
  replace ops newOp = newOp : filter ((/=newOp^.name) . (^.name)) ops
  fixedOp = findUnique c ops
  in case fixedOp of
       Just newOp -> findOp cs (c:done) (replace ops newOp) True
       Nothing -> findOp cs (c:done) ops False


step :: [Op] -> Instruction -> O ()
step ops (code, a, b, c) =
  let op = fromJust $ find ((==Just code).(^.opcode)) ops
  in op^.exec $ (a, b, c)


dayG :: IO ()
dayG = do
  i <- readFile "data/dayG.txt"
  let (expected, program) = fromJust $ parse parseFile i
  let go (before, after, _, args) = findEq before args after
  let m = go <$> expected
  let matches = length <$> m

  printf "Part1: %d\n" . length . filter (>2) $ matches

  let codedOps = findOp expected [] ops False
  let sequenced = traverse (step codedOps) program
  let res = execState sequenced [0, 0, 0, 0]

  printf "Part2: %d\n" $ head res
