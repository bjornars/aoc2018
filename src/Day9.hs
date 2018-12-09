{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Day9 where

import Data.Maybe
import Data.List hiding (cycle)
import Data.Foldable (toList)
import Data.Sequence (ViewL(..), Seq, viewl)

import Control.Monad
import Control.Monad.State
import Control.Lens hiding ((:<))

import qualified Data.Sequence as S
import qualified Data.Map as Map
import Text.Printf
import Debug.Trace

import Prelude hiding (cycle)

data GameState = GameState
  { _players :: Map.Map Int Integer
  , _current :: Int
  , _step :: Integer
  , _position :: Int
  , _marbles :: Seq Integer
  , _end :: Integer
  } deriving Show

makeClassy ''GameState

cycle :: Int -> Int -> Int -> Int
cycle _ 0 _ = 0
cycle _ 1 _ = 0
cycle pos size step | pos + step < 0  = cycle (pos + size) size step
cycle pos size step | pos + step >= size  = cycle (pos - size) size step
cycle pos _ step = pos + step

printSeq :: Seq Integer -> Int -> String
printSeq seq pos = concat
                   $ intersperse " "
                   $ (\(x, p) -> if p == pos
                                 then printf "(%2.1d)" x
                                 else printf " %2.1d " x)
                   <$> (zip (toList seq) [0..])

nextPos :: State GameState Int
nextPos = do
  pos <- use position
  num <- S.length <$> use marbles
  position <.= 1 + cycle pos num 1

nextPlayer :: State GameState ()
nextPlayer = do
  n <- Map.size <$> use players
  current %= ((`mod` n) . (+1))

printState :: State GameState ()
printState = do
  p <- use position
  c <- use current
  m <- use marbles
  traceM $ show c <> " " <> printSeq m p

game :: State GameState ()
game = do
  step' <- step <<+= 1
  -- printState
  nextPlayer
  if step' > 0 && step' `mod` 23 == 0
    then magic step'
    else placeStone step'

removeMarble :: State GameState Integer
removeMarble = do
  n <- S.length <$> use marbles
  p <- use position
  let pos = cycle p n (-7)
  position .= pos
  val <- (fromJust . S.lookup pos) <$> use marbles
  marbles %= S.deleteAt pos
  return val

placeStone :: Integer -> State GameState ()
placeStone step' = do
  pos' <- nextPos
  marbles %= (S.insertAt pos' step')
  end' <- use end
  unless (step' > end') game

magic :: Integer -> State GameState ()
magic step' = do
  current' <- use current
  value <- removeMarble
  players %= Map.adjust (+ (step' + value)) current'
  game

play :: Int -> Integer -> GameState
play n end = let
    players = Map.fromList $ zip [0 .. n-1] (repeat 0)
    init = (GameState players (n-1) 0 0 S.empty end)
    in execState game init

day9 :: IO ()
day9 = do
  solve (423, 71944)
  --solve (423, 71944 * 100)

solve (n, end) = do
  let res = play n end
  printf "%d players; last marble is worth %d points: high score is %d\n"
    n
    end
    (maximum $ res ^. players)
