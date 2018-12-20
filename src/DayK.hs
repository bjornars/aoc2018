module DayK where

-- import Control.Arrow
import Data.List (groupBy, maximumBy)
import Data.Function (on)
import Data.Maybe

import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)
import Lib

import Debug.Trace

data Dir = N | E | W | S | X deriving (Eq, Show, Read)
data Opts = Opts [Path] | Empty deriving (Show, Eq)
data Segment = Segment [Dir] Opts deriving (Show, Eq)
type Path = [Segment]

toDir :: Char -> Dir
toDir c = read [c]

lookAheadIn :: String -> ReadP ()
lookAheadIn str = do
  c <- head <$> look
  if c `elem` str
    then pure ()
    else pfail

parseStep :: ReadP Dir
parseStep = do
  c <- get
  --look >>= traceM  .(("read " <> show c <> " ") <> )
  case readMaybe [c] of
    Just res -> pure res
    Nothing -> pfail

parseAlternative :: ReadP Opts
parseAlternative = between (char '(') (char ')') $ do
  --look >>= traceM . ("before | " <>)
  x <- sepBy parseTerm (char '|')
  --look >>= traceM . ("after || " <>)
  lookAheadIn ")"
  --look >>= traceM . ("after | " <>)
  pure $ Opts x

parseTerm :: ReadP Path
parseTerm = do
  first <- parseSegment
  rest <- chainl (parseExtraSegment) (pure mappend) []
  return $ first : rest

parseSegment :: ReadP Segment
parseSegment = do
  --look >>= traceM . ("before preterm " <>)
  pre <- manyTill parseStep (lookAheadIn "()|$")
  --look >>= traceM . ("before alt " <>)
  options <- parseAlternative <++ pure Empty
  pure $ Segment pre options

parseExtraSegment :: ReadP [Segment]
parseExtraSegment = do
  --look >>= traceM . ("before preterm " <>)
  pre <- manyTill parseStep (lookAheadIn "()|$")
  --look >>= traceM . ("before alt " <>)
  options <- parseAlternative <++ pure Empty
  if null pre && options == Empty
    then pfail
    else pure [Segment pre options]

parseRegex = between (char '^') (char '$' >> skipSpaces >> eof) parseTerm

-- 
-- getOptions :: Opts -> [[Dir]]
-- getOptions (Opts xs) = concatMap allPaths $ xs
-- getOptions Empty = [[]]
-- 
-- allPaths :: Path -> [[Dir]]
-- -- allPaths (Path [] [] []) = [[X]]
-- allPaths (Path pre opts post) = (\opt -> (pre <> opt <> post)) <$> (concatMap getOptions opts)
-- 
-- longestPath re = let
--   p = fromJust $ parse parseRegex re
--   ps = allPaths p
--   in maximumBy (compare `on` length) ps

paths :: Segment -> [[Dir]]
paths (Segment pre Empty) = [pre]
paths (Segment pre (Opts paths)) = map (pre <>) $ concatMap findPaths paths

findPaths :: Path -> [[Dir]]
findPaths [] = [[]]
findPaths (x:xs) = [p <> ps | p <- paths x, ps <- findPaths xs]

findBiggest = length . maximumBy (compare `on` length) . findPaths . fromJust . parse parseRegex

dayK :: IO ()
dayK = do
  print $ findBiggest "^ENWWW(NEEE|SSE(EE|N))$"
  print $ findBiggest "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"
  print $ findBiggest "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"


  i <- readFile "data/dayK.txt"
  print $ findBiggest i
  -- let p = fromJust $ parse parseRegex i
  -- print p
  -- print $ i
  --let p = longestPath i
  --print $ p
  --print $ length p
