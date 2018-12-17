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


data Op = Op { _name :: String
             , _opcode :: Maybe Int
             , _exec :: (Int, Int, Int) -> O () }

instance Show Op where
  show (Op name code _) = "Op(" <> show name <> ", " <> show (fromMaybe (-1) code) <> ")" 
makeLenses ''Op

rr, ri, ir :: (Int -> Int -> Int) -> (Int, Int, Int) -> O ()
rr binop (x, y, c) = binop <$> load x <*> load y >>= store c
ri binop (x, y, c) = binop <$> load x <*> pure y >>= store c
ir binop (x, y, c) = binop <$> pure x <*> load y >>= store c

flag :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
flag binop  = binop >:> bool 0 1

ops :: [Op]
ops =
   [Op "addr" Nothing (rr (+))
  , Op "addi" Nothing (ri (+))
  , Op "mulr" Nothing (rr (*))
  , Op "muli" Nothing (ri (*))
  , Op "banr" Nothing (rr (.&.))
  , Op "bani" Nothing (ri (.&.))
  , Op "borr" Nothing (rr (.|.))
  , Op "bori" Nothing (ri (.|.))
  , Op "setr" Nothing (\(x, _, c) -> load x >>= store c)
  , Op "seti" Nothing (\(x, _, c) -> pure x >>= store c)
  , Op "gtir" Nothing (ir (flag (>)))
  , Op "gtri" Nothing (ri (flag (>)))
  , Op "gtrr" Nothing (rr (flag (>)))
  , Op "eqir" Nothing (ir (flag (==)))
  , Op "eqri" Nothing (ri (flag (==)))
  , Op "eqrr" Nothing (rr (flag (==)))
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
