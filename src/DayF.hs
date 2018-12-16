{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -Wall              #-}

module DayF where

import           Control.Lens        hiding (Empty, from, ix, to)
import           Control.Monad
import           Control.Monad.State
import           Data.Array          hiding (inRange)
import           Data.Either
import           Data.Foldable (toList)
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Sequence       (Seq ((:<|), Empty), (><))
import qualified Data.Sequence       as Q
import qualified Data.Set            as S
import           Text.Printf

import           Debug.Trace         hiding (traceM)
import           Lib                 (groupSort)
import           System.IO.Unsafe

sample0, sample1, sample2, sample3, sample4, sample5 :: [String]
sample0 = [
   "#######"
  , "#.G...#"
  , "#...EG#"
  , "#.#.#G#"
  , "#..G#E#"
  , "#.....#"
  , "#######"
  ]

sample1 = [
  "#######",
  "#G..#E#",
  "#E#E.E#",
  "#G.##.#",
  "#...#E#",
  "#...E.#",
  "#######"
  ]

sample2 = [
  "#######",
  "#E..EG#",
  "#.#G.E#",
  "#E.##E#",
  "#G..#.#",
  "#..E#.#",
  "#######"
  ]

sample3 = [
  "#######",
  "#E.G#.#",
  "#.#G..#",
  "#G.#.G#",
  "#G..#.#",
  "#...E.#",
  "#######"
  ]

sample4 = [
  "#######",
  "#.E...#",
  "#.#..G#",
  "#.###.#",
  "#E#G#G#",
  "#...#G#",
  "#######"
  ]

sample5 = [
  "#########",
  "#G......#",
  "#.E.#...#",
  "#..##..G#",
  "#...##..#",
  "#...#...#",
  "#.G...G.#",
  "#.....G.#",
  "#########"
  ]

type Coord = (Int, Int)

data Faction = Elf | Goblin deriving (Show, Eq)
makePrisms ''Faction

data Actor = Actor { _faction :: Faction,
                     _coords  :: Coord,
                     _hp      :: Int
                   } deriving (Eq, Show)

data BType  = Wall | Open deriving Eq
instance Show BType where
  show Wall = "#"
  show Open = "."

type Board = Array Coord BType
data Game = Game { _actors :: [Actor],
                   _board  :: Board,
                   _ticks  :: Int,
                   _done   :: Bool
                 } deriving Show

makeLenses ''Actor
makeClassy ''Game
parseChar :: Char -> Either Char BType
parseChar '#' = Right Wall
parseChar '.' = Right Open
parseChar 'G' = Left 'G'
parseChar 'E' = Left 'E'
parseChar _   = undefined

type M a = StateT Game IO a

traceM :: String -> M ()
traceM = liftIO . putStrLn

showc :: Coord -> String
showc a = let (x, y) = a in printf "(%d, %d)" x y
showa :: Actor -> String
showa a = printf "%c%s " (if a^.faction == Goblin then 'G' else 'E') (showc (a^.coords))

printa :: Actor -> String -> M ()
printa a str = pure () -- traceM $ showa a <> str

getActor :: ((Int, Int), Either Char a) -> Maybe Actor
getActor (ix, Left 'E') = Just $ Actor Elf ix 200
getActor (ix, Left 'G') = Just $ Actor Goblin ix 200
getActor _              = Nothing

parse :: [String] -> Game
parse xss =
  let
    sq = fmap parseChar . concat . transpose $ xss
    actorless = fromRight Open <$> sq
    dims = (length xss - 1, length (head xss) - 1)
    actarr = listArray ((0, 0), dims) sq
    as = mapMaybe getActor $ assocs actarr
  in
    Game as (listArray ((0, 0), dims) actorless) 0 False

showGame :: Game -> [String]
showGame (Game as b tick _) =
  let
      goblins' = filter (has (faction._Goblin)) as
      elves' = filter (has (faction._Elf)) as
      goblins'' =(,"G") . view coords <$> goblins'
      elves'' =(,"E") . view coords <$> elves'
      combined = (show <$> b) // elves''  // goblins''
      transposed = groupSort (snd.fst) (assocs combined)
      stripped = (concat <$>) $ (fmap.fmap) snd transposed
      pprint = uncurry $ printf "%c(%d)"
      scores c a = intercalate ", " (pprint . (c, ) . view hp <$> a)
  in [printf "Turn: %d" tick]
      <> stripped
      <> [scores 'E' elves', scores 'G' goblins']
      <> [""]

printGame :: Game -> IO ()
printGame = mapM_ putStrLn . showGame

playOrder :: [Actor] -> [Actor]
playOrder = sortBy (compare `on` (^.coords.swapped))

-- possible moves in reading-order
inRange :: Coord -> [Coord]
inRange (x, y) = [(x, y-1),(x-1, y),(x+1, y),(x, y+1)]

findPath :: Board -> [Actor] -> Coord -> [Coord] -> Coord
findPath b as start goals =
  let
      openBoard = map fst . filter ((==Open) . snd) $ assocs b
      occupied =  (^.coords) <$> as
      next = bfs start (openBoard \\ occupied) goals
  in fromMaybe start next


-- find an enemy in swing-order
swing :: [Coord] -> [Actor] -> Actor
swing (s:sz) enemies = fromMaybe (swing sz enemies)
                       $ find ((==s) . (^.coords)) enemies
swing [] _ = undefined


update :: Eq a => a -> a -> [a] -> [a]
update from to (x:xs) | x == from = to : xs
update from to (x:xs) = x: update from to xs
update _ _ []         = []


attack :: Actor -> [Actor] -> [Actor] -> [Actor] -> M [Actor]
attack a xs zs enemies = do
  let swings = inRange $ a ^. coords
  -- find the dyiest enemy in range
  let targets = groupSort (^.hp) .  filter ((`elem` swings).(^.coords)) $ enemies
  case targets of
    (targets':_) ->  do
      -- when (length targets > 1) $ traceM $ show a <> " have " <> show tt
      -- when (length targets > 1) $ traceM $ show a <> " aming at " <> show targets
      let v = swings `swing` targets'
      let (res, v') = v & hp <-~ 3
      --traceM $ show a <> " swings at " <> show v <> ", and became " <> show v'
      if True && res >= 0
        then do
          printa a $ "attack " <> showa v <> show res
          processTurn (update v v' xs) (a : update v v' zs)
        else do
          printa a $ "attack " <> showa v <> "XXX"
          processTurn (delete v xs) (a : delete v zs)

    -- noone in range, skip
    _ -> processTurn xs (a:zs)


move :: Actor -> [Actor] -> [Actor] -> [Coord] -> [Actor] -> M [Actor]
move a xs zs targets enemies = do
  b <- use board
  let next = findPath b (xs <> zs) (a ^. coords) targets
  if next == a^.coords
    then pure ()
    else printa a $ "move to " <> showc next
  attack (a & coords .~ next) xs zs enemies


processTurn :: [Actor] -> [Actor] -> M [Actor]
processTurn [] zs = pure zs
processTurn (a : xs) zs = do
    -- traceM $ "processing " <> show a
    let others = xs <> zs
    let f = a^.faction
    let enemies = filter ((/=f) . (^.faction)) others
    let neigs = inRange (a ^. coords)
    b <- use board

    let occupied = (^.coords) <$> others
    let goals = nub
                . filter (`notElem` occupied)
                . filter ((== Open) . (b!))
                . concatMap inRange
                . map (^.coords)
                $ enemies

    -- traceM $ "can reach " <> show neigs
    -- traceM $ "enemies at " <> show ((^. coords) <$> enemies)

    let enemiesInRange = any (`elem` neigs) ((^.coords) <$> enemies)

    if a^.hp <= 0
      then processTurn xs (a:zs)
    else if null enemies
      then do
        done .= True
        pure $ a : (xs <> zs)

    else if null goals -- && not enemiesInRange
      then printa a "skip" >> processTurn xs (a:zs)
    else if not enemiesInRange
      then move a xs zs goals enemies
    else
      attack a xs zs enemies

play :: M ()
play = do
  -- play one round
  t <- use ticks
  -- traceM $ "Round " <> show t
  as <- use actors
  as' <- processTurn (playOrder as) []
  actors .= filter ((>0) . (^.hp)) as'

  -- traceM $ replicate 20 '-'
  d <- use done
  when (not d) $ do
    ticks +=1
    -- use ticks >>= traceM . ("start round " <> ). show
    play

dayF :: IO ()
dayF = do
  -- run (pure sample0) "27730"
  -- run (pure sample1) "36344"
  -- run (pure sample2) "39514"
  -- run (pure sample3) "27755"
  -- run (pure sample4) "28944"
  -- run (pure sample5) "18740"
  run (lines <$> readFile "data/dayF.txt") "346574 XXX"

run :: IO [String] -> String -> IO ()
run input expected = do
  g <- parse <$> input >>= execStateT play
  print $ g^.ticks
  print $ sum ((^.hp) <$> (g^.actors))
  printf "Part1:    %d\n" $ g^.ticks * sum ((^.hp) <$> (g^.actors))
  printf "Expected: %s\n" expected


bfs :: Coord -> [Coord] -> [Coord] -> Maybe Coord
bfs start open goals = go S.empty (Q.singleton [start])
  where go :: S.Set Coord -> Q.Seq [Coord] -> Maybe Coord
        go _ Empty = Nothing
        go seen (check :<| todo) =
          let l = last check
              lWin = if l `elem` goals then [check] else []
              swop (x, y) = (y, x)
              allWins = lWin <> toList (Q.filter (\t -> last t `elem` goals && length t == length check) todo)
              bestWin = sortBy (compare `on` (swop . last)) allWins
          in
          if not $ null bestWin
            then Just $ (head bestWin) !! 1
            else let x = Q.fromList . filter (`S.notMember` seen)
                        . filter (`elem` open) $ inRange l
                     addCheck n = check <> [n]
                in go (foldr S.insert seen x) (todo >< (addCheck <$> x))
