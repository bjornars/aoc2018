{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall              #-}

module DayJ where

import           Data.Bits
import           Data.Bool
import           Data.List
import qualified Data.Map                     as M
import           Data.Maybe

import           Control.Lens
import           Control.Monad.RWS

import           Lib
import           Text.ParserCombinators.ReadP hiding (get)
import qualified Text.ParserCombinators.ReadP as RP
import           Text.Printf


type Instruction = (String, Int, Int, Int)
type Ram = [Int]
data Rom = Rom { _ipReg :: Int, _opcodes :: M.Map String Op, _program :: [Instruction] }

type O a = RWST Rom () Ram IO a

newtype Op = Op { _getOp :: (Int, Int, Int) ->  O () }
makeLenses ''Op
makeLenses ''Rom

type Ops = M.Map String Op
type Mnem = String

load :: Int -> O Int
load addr = gets (^?! ix addr)

store :: Int -> Int -> O ()
store addr val = modify (set (ix addr) val)

rr, ri, ir :: (Int -> Int -> Int) -> (Int, Int, Int) -> O ()
rr binop (x, y, c) = binop <$> load x <*> load y >>= store c
ri binop (x, y, c) = binop <$> load x <*> pure y >>= store c
ir binop (x, y, c) = binop <$> pure x <*> load y >>= store c

flag :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
flag binop  = binop >:> bool 0 1

ops :: M.Map String Op
ops = M.fromList [ ("addr", Op (rr (+)))
                 , ("addi", Op (ri (+)))
                 , ("mulr", Op (rr (*)))
                 , ("muli", Op (ri (*)))
                 , ("banr", Op (rr (.&.)))
                 , ("bani", Op (ri (.&.)))
                 , ("borr", Op (rr (.|.)))
                 , ("bori", Op (ri (.|.)))
                 , ("setr", Op (\(x, _, c) -> load x >>= store c))
                 , ("seti", Op (\(x, _, c) -> pure x >>= store c))
                 , ("gtir", Op (ir (flag (>))))
                 , ("gtri", Op (ri (flag (>))))
                 , ("gtrr", Op (rr (flag (>))))
                 , ("eqir", Op (ir (flag (==))))
                 , ("eqri", Op (ri (flag (==))))
                 , ("eqrr", Op (rr (flag (==))))
                 ]

parseRow :: ReadP Instruction
parseRow = do
  skipSpaces
  a <- manyTill RP.get (char ' ')
  b <- digit <* char ' '
  c <- digit <* char ' '
  d <- digit <* char '\n'
  return (a,b,c,d)

parseFile :: ReadP (Int, [Instruction])
parseFile = do
  string "#ip "
  ipReg <- digit <* skipSpaces
  program <- many1 parseRow
  eof
  return (ipReg, program)


trace, traceLn :: MonadIO m => String -> m ()
trace str = when (False) $ liftIO . putStr $ str
traceLn str = when (False) $ liftIO . putStrLn $ str

step :: Int -> O ()
step ip = do
  ir <- view ipReg
  next <- preview $ program . (ix ip)

  case next of
    Just (mn, a, b, c) -> do
      store ir ip
      trace $ "ip=" <> show ip <> " "
      get >>= trace . show
      trace $ " " <> show (mn, a, b, c) <> " "
      i <- (M.! mn) <$> view opcodes
      i^.getOp $ (a, b, c)
      get >>= traceLn . show
      newIp <- load ir

      if (newIp == 1)
        then do
          r2 <- load 2
          if r2 > 10000
            then pure () -- omg!
            else step (newIp + 1)
        else step (newIp + 1)

    Nothing -> pure () -- instruction underrun

dayJ :: IO ()
dayJ = do
  i <- readFile "data/dayJ.txt"
  let (ipReg, program) = fromJust $ parse parseFile i
  let rom = Rom ipReg ops program

  (res, _) <- execRWST (step 0) rom [0,0,0,0,0,0]
  printf "Part1: %d\n" (res !! 0)

  (res, _) <- execRWST (step 0) rom [1,0,0,0,0,0]
  let fuckyprime = res !! 2
  let factors = prime_factors fuckyprime
  let divisors = get_divisors factors
  printf "Part2: %d\n" (sum divisors)

prime_factors :: Int -> [Int]
prime_factors 1 = []
prime_factors n
  | factors == []  = [n]
  | otherwise = factors ++ prime_factors (n `div` (head factors))
  where factors = take 1 $ filter (\x -> (n `mod` x) == 0) [2 .. n-1]

get_divisors :: [Int] -> [Int]
get_divisors xs = product . nub <$> subsequences xs
