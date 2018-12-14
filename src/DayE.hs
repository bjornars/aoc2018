{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module DayE where

import Data.Sequence as S hiding ((|>))
import Debug.Trace

import Text.Printf

import Control.Lens
import Control.Monad.State

data Kitchen = Kitchen { _rxs :: S.Seq Int
                       , _elfs :: (Int, Int)
                       } deriving Show
makeLenses ''Kitchen

rxAtPos :: Int -> State Kitchen Int
rxAtPos idx = (`S.index` idx) <$> use rxs

circulate :: Int -> Int -> Int -> Int
circulate a n c = (1 + c + a) `mod` n

recipize :: Int -> State Kitchen (S.Seq Int)
recipize lim = do
  -- (,) <$> use elfs <*> use rxs >>= traceShowM
  (e1, e2) <- use elfs
  crx1 <- rxAtPos e1
  crx2 <- rxAtPos e2

  let (s1, s2) = (crx1 + crx2) `divMod` 10

  unless (s1 == 0) $
    rxs %= (|> s1)
  rxs %= (|> s2)

  slen <- S.length <$> use rxs
  elfs._1 %= circulate crx1 slen
  elfs._2 %= circulate crx2 slen

  if slen >= (10 + lim)
    then use rxs >>= pure . S.take 10 . S.drop lim
    else recipize lim

dayE :: IO ()
dayE = do
  let freshKitchen = Kitchen (S.fromList [3,7]) (0, 1)
  let res = evalState (recipize 430971) freshKitchen
  printf "Part 1: %s\n" $ concat $ show <$> res
