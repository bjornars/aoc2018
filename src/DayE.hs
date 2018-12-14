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

endsWith :: Eq a => Seq a -> Seq a -> Bool
endsWith haystack needle = let hl = S.length haystack in
  S.drop (hl - S.length needle) haystack == needle

dropR :: Seq a -> Seq a
dropR (xs :|> _) = xs
dropR _ = undefined

recipize :: S.Seq Int -> State Kitchen Int
recipize goal = do
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

  r <- use rxs
  if r `endsWith` goal
    then pure $ S.length r - S.length goal
    else
      -- special handling for when we add two recipes at the same time
      if s1 /= 0 && dropR r `endsWith` goal
        then pure $ S.length r - S.length goal - 1
    else recipize goal

dayE :: IO ()
dayE = do
  let freshKitchen = Kitchen (S.fromList [3,7]) (0, 1)
  let res = evalState (recipize $ S.fromList [4,3,0,9,7,1]) freshKitchen
  printf "Part 2: %s\n" $ show res
