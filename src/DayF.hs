{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module DayF where

import           Data.Array
import           Data.Either
import           Data.Function
import           Data.List
import           Data.Maybe
import           Control.Lens
import           Control.Monad
import           Text.Printf

import           Debug.Trace

sample = [
  "#######",
  "#G..#E#",
  "#E#E.E#",
  "#G.##.#",
  "#...#E#",
  "#...E.#",
  "#######"
  ]

--Combat ends after 37 full rounds
--Elves win with 982 total hit points left
--Outcome: 37 * 982 = 3633

type Coord = (Int, Int)

data Actor = Actor { _coords :: Coord,
                     _hp     :: Int
                   } deriving (Show, Ord, Eq)

data BType  = Wall | Open
instance Show BType where
  show Wall = "#"
  show Open = "."

type Board = Array Coord BType
data Game = Game { _elves   :: [Actor],
                   _goblins :: [Actor],
                   _board   :: Board,
                   _ticks   :: Int
                 } deriving Show

makeLenses ''Actor
makeClassy ''Game
parseChar :: Char -> Either Char BType
parseChar '#' = Right Wall
parseChar '.' = Right Open
parseChar 'G' = Left 'G'
parseChar 'E' = Left 'E'

getElf, getGoblin :: ((Int, Int), Either Char a) -> Maybe Actor
getElf (ix, Left 'E') =     Just $ Actor ix 200
getElf _                =     Nothing
getGoblin (ix, Left 'G') = Just $ Actor ix 200
getGoblin _                = Nothing

parse :: [String] -> Game
parse xss =
  let
    sq = fmap parseChar . concat . transpose $ xss
    actorless = fromRight Open <$> sq
    dims = (length xss, length (head xss))
    actarr = listArray ((1, 1), dims) sq
    elves = mapMaybe getElf $ assocs actarr
    goblins = mapMaybe getGoblin $ assocs actarr
  in
    Game (sort elves) (sort goblins) (listArray ((1, 1), dims) actorless) 0

printGame :: Game -> IO ()
printGame (Game elves goblins board tick) =
  let combined = (show <$> board) // elves'  // goblins'
      goblins' = (,"G") . view coords <$> goblins
      elves' = (,"E") . view coords <$> elves
      groupSort key = groupBy ((==) `on` key) . sortBy (compare `on` key)
      transposed = groupSort (snd.fst) (assocs combined)
      stripped = (concat <$>) $ (fmap.fmap) snd transposed
      print = uncurry $ printf "%c(%d)"
      scores c a = intercalate ", " (print . (c, ) . view hp <$> a)
  in mapM_ putStrLn $ stripped <> [scores 'E' elves, scores 'G' goblins]

dayF = do
  let game = parse sample
  printGame game
