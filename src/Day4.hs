{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

module Day4 where

import Control.Lens
import Control.Monad
import Control.Monad.State
import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Function
import Data.List
import qualified Data.Map as M
import Text.ParserCombinators.ReadP hiding (get)
import Text.Read
import qualified Text.ParserCombinators.ReadP as P

data Shift = Start String
           | Sleep Integer
           | Wakeup Integer deriving (Show)

data S = S { _current :: String
           , _bedtime :: Integer
           , _slumbers :: [(String, Integer, Integer)] } deriving Show

makeClassy ''S

doParse :: String -> _
doParse line = flip readP_to_S line $ do
  char '['
  [year, month, day] <- sepBy1 digits (char '-')
  char ' '
  [hour, minutes] <- sepBy1 digits (char ':')
  string "] "
  let time = minutes
  text <- word
  ret <- case text of
    "Guard" -> char ' ' >> Just . Start <$> word
    "falls" -> return . Just $ Sleep time
    "wakes" -> return . Just $ Wakeup time
    r -> return Nothing
  chomp
  return $ ((year, month, day, hour, minutes),) <$> ret

  where digits = read @Integer <$> munch1 isDigit
        word = munch1 $ not . isSpace
        chomp = manyTill P.get eof

parse line = case doParse line of
  [(Just res, [])] -> Just res
  r -> Nothing

sleeps :: Shift -> State S ()
sleeps (Start guard) = current .= guard
sleeps (Sleep time) = bedtime .= time
sleeps (Wakeup time) = do
  g <- use current
  b <- use bedtime
  slumbers %= ((g, time - b, b):)

day4 :: IO ()
day4 = do
  shifts <- traverse parse . lines <$> readFile "data/day4.txt"
  case shifts of
    Nothing -> print "Parse error"
    Just s -> do
      let sorted = map snd . sortBy (compare `on` fst) $ s
      let schedule = traverse sleeps sorted
      let initState = S "#-1" 0 []
      let fst3 = (^. _1) :: (a, b, c) -> a
      let snd3 = (^. _2) :: (a, b, c) -> b
      let thr3 = (^. _3) :: (a, b, c) -> c
      let naps = execState schedule initState ^. slumbers
      let naps_on_guard = groupBy ((==) `on` fst3) . sort $ naps
      let naps_summed = (\x -> (fst3 (head x), sum $ map snd3 x)) <$> naps_on_guard
      let (nappy_guard, total_napped) = maximumBy (compare `on` snd) naps_summed
      let sleepiest_minute =
            map M.fromList $
            map (\(_, dur, start) -> [(x, Sum 1) | x <- [start..dur+start-1]]) $
            filter ((== nappy_guard) . fst3) naps
      let napmap = foldl' (M.unionWith (<>)) M.empty sleepiest_minute
      let nappy_minute = fst $ maximumBy (compare `on` snd) $ M.toList napmap
      putStrLn $ nappy_guard <> " is sleepy"
      putStrLn $ show nappy_minute <> " is sleepy"
      print $ read @Integer (tail nappy_guard) * nappy_minute
