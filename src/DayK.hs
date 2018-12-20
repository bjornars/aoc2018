{-# OPTIONS_GHC -Wall #-}

module DayK where

-- import Control.Arrow
import           Data.Bifunctor
-- import Data.Traversable
import           Data.Foldable
import           Data.Maybe
import qualified Data.Sequence                as S
import qualified Data.Set                     as M

import           Control.Monad.State

import           Lib
import           Text.ParserCombinators.ReadP hiding (get)
import qualified Text.ParserCombinators.ReadP as RP
import           Text.Read                    (readMaybe)

import           Debug.Trace

data Dir = N | E | W | S deriving (Eq, Show, Read)
data Opts = Opts [Path] | Empty deriving (Show, Eq)
data Segment = Segment (S.Seq Dir) Opts deriving (Show, Eq)
type Path = [Segment]

type Coords = (Int, Int)
type Room = M.Set Coords
type St s = (Coords, Room) -> s -> (Coords, Room)

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
  c <- RP.get
  --look >>= traceM  .(("read " <> show c <> " ") <> )
  case readMaybe [c] of
    Just res -> pure res
    Nothing  -> pfail

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
  pure $ Segment (S.fromList pre) options

parseExtraSegment :: ReadP [Segment]
parseExtraSegment = do
  --look >>= traceM . ("before preterm " <>)
  pre <- manyTill parseStep (lookAheadIn "()|$")
  --look >>= traceM . ("before alt " <>)
  options <- parseAlternative <++ pure Empty
  if null pre && options == Empty
    then pfail
    else pure [Segment (S.fromList pre) options]

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

-- paths :: Segment -> [S.Seq Dir]
-- paths (Segment pre Empty) = [pre]
-- paths (Segment pre (Opts paths)) = fmap (pre S.><) $ mconcat (fmap findPaths paths)
--
-- findPaths :: Path -> M.Set (Int,Int)
-- findPaths = walkSegment M.empty (0, 0)

type S a = StateT Room IO a

step :: Dir -> Coords -> Coords
step N = second pred
step S = second succ
step W = first pred
step E = first succ

-- walkSeg :: St Dir
-- walkSeg (cur, rooms) dir = let next = dir `step` cur in (next, next `M.insert` rooms)
-- 
-- walkSegment :: St Segment
-- walkSegment (cur, rooms) (Segment pre opts) = let
--   n = foldl' walkSeg (cur, rooms) (traceShowId pre)
--   in undefined -- fold' walkOpts
-- 
-- walkPath :: St Path
-- walkPath (cur, rooms) xs = foldl' walkSegment (cur, rooms) xs
-- --rooms -- [p <> ps | p <- paths x, ps <- findPaths xs]
-- 
-- walk :: Path -> Room
-- walk = snd . walkPath ((0, 0), M.singleton (0, 0))
-- 
-- findBiggest = walk . fromJust . parse parseRegex

walkStep :: Coords -> Dir -> S Coords
walkStep c dir = do
  let next = step dir c
  g <- get
  unless (next `M.member` g) $ liftIO . putStrLn $  "plop " <> show (M.size g) <> " : " <> show next
  modify (next `M.insert`)
  pure next

walkSeg :: Coords -> Path -> S ()
walkSeg _ [] = pure ()
walkSeg start (Segment pre opts: rest) = do
  -- liftIO . putStrLn $ "walkseg " <> show (length rest)
  end <- foldM walkStep start pre
  walkOpts end opts rest

walkOpts :: Coords -> Opts -> Path -> S ()
walkOpts start Empty path = walkSeg start path
walkOpts start (Opts opts) path = forM_ opts (\x -> walkSeg start (x <> path))

printMap :: M.Set (Int, Int) -> [String]
printMap rooms = let
  (minx, maxx) = minmax (fst <$> M.elems rooms)
  (miny, maxy) = minmax (snd <$> M.elems rooms)
  in [
      [ if (x, y) == (0,0) then 'O' else if (x, y) `M.member` rooms then 'X' else  '.'
      | x <- [minx..maxx]]
    | y <- [miny..maxy]]

walk :: Path -> IO Room
walk path = execStateT (walkSeg (0, 0) path) M.empty

-- findBiggest = walk . fromJust . parse parseRegex

dayK :: IO ()
dayK = do
  -- print $ findBiggest "^ENWWW(NEEE|SSE(EE|N))SSSSSSSSSS$"
  -- traverse_ putStrLn $  printMap $ findBiggest "^ENWWW(NEEE|SSE(EE|N))SSSSS$"
  -- print $ findBiggest "^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$"

  -- traverse_ putStrLn $  printMap $ findBiggest "^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$"

  i <- readFile "data/dayK.txt"
  let p = fromJust . parse parseRegex $ i
  print $ p
  print $ length p
  res <- walk p
  traverse_ putStrLn $  printMap $ res


  -- i <- readFile "data/dayK.txt"
  -- print $ findBiggest i
  -- let p = fromJust $ parse parseRegex i
  -- print p
  -- print $ i
  --let p = longestPath i
  --print $ p
  --print $ length p
