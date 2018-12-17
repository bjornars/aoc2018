{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# OPTIONS_GHC -Wall   #-}

module DayH where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Lens hiding ((...), over, under)

import Data.Array
import Data.Maybe
import Data.Function (on)
import Data.List (groupBy, sortBy)
import qualified Data.Set as S
import Text.ParserCombinators.ReadP hiding (get)
import qualified Text.ParserCombinators.ReadP as RP

import Debug.Trace

import Lib

type Coord = (Int, Int)
data Vein = Vein { _vx :: Coord, _vy :: Coord } -- x-span, y-span, inclusive
  deriving Show

parseVein :: ReadP (Char, Int, Int)
parseVein = do
  t <- RP.get
  void $ char '='
  start <- digit
  end <- (string ".." >> digit) <|> pure start
  return (t, start, end)

parseLine :: ReadP Vein
parseLine = do
  (t1, s1, e1) <- parseVein
  void $ string ", "
  (_, s2, e2) <- parseVein
  eof
  pure $ if t1 == 'x'
    then Vein (s1, e1) (s2, e2)
    else Vein (s2, e2) (s1, e1)

data Block = Still | Running | Sand | Clay deriving Eq
instance Show Block where
   show Sand = " "
   show Still = "-"
   show Running = "|"
   show Clay = "#"

type W = S.Set Coord
type B = Array Coord Block
data SS = SS { _board :: B, _wrun :: W, _wstill :: W }
makeClassy ''SS

type G a = StateT SS IO a

still :: [Coord] -> G ()
still ixs = do
  let n = (S.fromList ixs)
  wstill %= S.union n
  wrun %= (S.\\ n)

running :: [Coord] -> G ()
running ixs = do
  let n = (S.fromList ixs)
  wrun %= S.union n
  wstill %= (S.\\ n)

readGame :: [Vein] -> B
readGame vs = emptyArr // ((,Clay) <$> concatMap clays vs)
  where
    clays (Vein (x1, x2) (y1, y2))  = ([(y, x) | y <- [y1..y2], x <- [x1..x2]])
    emptyArr = listArray ((0, xmin-10), (ymax, xmax+10)) $ repeat Sand
    extremes = minmax . concatMap (\(a, b) -> [a, b]) ...  map
    (xmin, xmax) = extremes _vx vs
    (_, ymax) = extremes _vy vs

logWait :: G ()
logWait = do
  when (False) $ do
    printGame
    void $ liftIO getLine

logPrint :: String -> G ()
logPrint str = do
  when (False) $ do
    liftIO $ putStrLn str

fillDown :: Coord -> G ()
fillDown bb@(origY, origX) = do
   logWait
   logPrint $ "filling down" <> show bb
   -- poor straight down to the ground
   b <- use board
   ww <- use wstill
   wr <- use wrun

   let bedrock = fst.snd $ bounds b
   let blocks = [(y, origX) | y <- [origY .. bedrock]]
   --liftIO . print  $ show [origY, origX, bedrock]
   let rayToGround  = takeWhile
                        (\c -> (b ! c == Sand) && c `S.notMember` ww && c `S.notMember` wr)
                        blocks
   -- liftIO . print $ "ray to ground" <> show rayToGround
   running rayToGround
   if (null rayToGround)
     then pure () -- nowhere to fill
    else if fst (last rayToGround) == bedrock
     then pure ()
    else if (last rayToGround) `S.member` wr
     then pure ()
     else spill $ last rayToGround

spill :: Coord -> G ()
spill bb@(bucketY, bucketX) = do
  logWait
  logPrint $ "spilling" <> show bb
  -- get >>= printGame
  -- spill sideways until we have clay on the side or sand underneath
  b <- use board
  ws <- use wstill
  wr <- use wrun

  -- liftIO . putStrLn $ "spill from " <> show bb <> ", " <> show (bounds b)
  let ((_, minX), (_, maxX)) = bounds b
  let takeSand = takeWhile ((==Sand).(b!))
  -- liftIO . putStrLn $ "b " <> show (bounds b)
  -- liftIO . putStrLn $ "scanning " <> show [(bucketY, x) | x <- [minX..bucketX]] <> " - " <> show [(bucketY, x) | x <- [bucketX+1..maxX]]
  let toLeft = reverse $ takeSand [(bucketY, x) | x <- reverse [minX..bucketX]]
  let toRight = takeSand [(bucketY, x) | x <- [bucketX+1..maxX]]
  let wet = toLeft <> toRight
  -- liftIO . print  $ "wet " <> show wet
  -- liftIO . print  $ "left " <> show toLeft <> " " <> show toRight
  let doesntRunOff = (bucketY, minX) `notElem` wet  && (bucketY, maxX) `notElem` wet

  let solidUnderneath coord = b !coord == Clay || coord `S.member` ws || coord `S.member` wr
  if all (solidUnderneath . (first succ)) wet && doesntRunOff
    -- its all solid underneath, fill up, and we didnt run into the bounds
    then do
      logPrint $ "spilfil " <> show wet
      still wet
      spill (bucketY-1, bucketX)
    else do
      spillSideways ( 1) bb
      spillSideways (-1) bb

spillSideways :: Int -> Coord -> G ()
spillSideways dir bb@(bucketY, bucketX) = do
  logWait
  logPrint $ "spilling side " <> show dir <> ", " <> show bb
  b <- use board
  let bounds' = bounds b
  let next = (bucketY, bucketX + dir)
  let over = (bucketY - 1, bucketX)
  let under = (bucketY + 1, bucketX)
  ws <- use wstill
  wr <- use wrun

  -- liftIO . putStrLn $ "spill sideways" <> show dir <> " " <> show bb
  -- liftIO . putStrLn $ show wr

  if bb `S.member` ws
    then pure ()

  else if not $ inRange bounds' bb || bucketY == (fst.snd) bounds'
    then pure ()

  else if b ! bb == Clay
    then pure ()

  else if under `S.member` wr && over `S.notMember` wr
    then pure ()

  else if under `S.notMember` ws && under `S.notMember` wr && b ! under == Sand
    then fillDown bb

  else running [bb] >> spillSideways dir next

printGame :: G ()
printGame = do
  bb <- use board
  wr <- S.toList <$> use wrun
  ws <- S.toList <$> use wstill
  let watered = (bb // ((,Still) <$> ws))  // ((,Running) <$> wr)
  let blocks = groupBy ((==) `on` (fst.fst)). sortBy (compare `on` (fst.fst))
                  -- . take 100000
                  . assocs $ watered
  let pBlock = show . snd
  let lines' = (fmap.concatMap) pBlock blocks
  (liftIO $ putStrLn "") >> mapM_ (liftIO .putStrLn)
      (
      -- take 100
      lines'
      )

dayH :: IO ()
dayH = do
  input <- lines  <$> readFile "data/dayH.txt"
  let input1 = [ "x=495, y=2..7",
                "y=7, x=495..501",
                "x=501, y=3..7",
                "x=498, y=2..4",
                "x=506, y=1..2",
                "x=498, y=10..13",
                "x=504, y=10..13",
                "y=13, x=498..504"
            ]

  -- print $ traverse (parse parseLine) input
  print $ fmap length . traverse (parse parseLine) $ input
  let game = readGame . fromJust . traverse (parse parseLine) $ input
  let fresh = SS game S.empty S.empty
  print $ bounds game
  -- void $ evalStateT printGame fresh
  -- print . take 100 $ assocs game
  game' <- execStateT (fillDown (0, 500)) fresh
  void $ evalStateT printGame game'
  let smallestY = minimum $ fmap (fst.fst) . filter ((==Clay) . snd) . assocs $ game'^.board
  let waters = (game'^. wstill) `S.union` (game'^. wrun)

  putStr "Part1: "
  print . S.size . S.filter ((>=smallestY).fst) $ waters

  putStr "Part2: "
  print . S.size . S.filter ((>=smallestY).fst) $ (game'^. wstill)
