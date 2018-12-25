{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE TypeApplications         #-}
{-# OPTIONS_GHC -Wall              #-}

module DayO where

import Control.Arrow ((***))
import Control.Monad (void)
import Control.Lens hiding ((...))
import Control.Monad.State

import Data.Bool
import Data.Function (on)
import Data.List (delete, find, elemIndex, sortBy, maximumBy)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Debug.Trace hiding (traceM)

import Text.ParserCombinators.ReadP
import Text.Printf
import Lib

data DmgType = Blunt | Sharp | Cold | Fire | Radiation deriving (Eq, Show)
data Faction = Good | Bad deriving (Eq, Show)
type Weakness = [DmgType]
type Immunity = [DmgType]
data Units = Units {
  _uid :: Int,
  _ufaction :: Faction,
  _uno :: Int,
  _uhp :: Int,
  _uinit :: Int,
  _uweakness :: [DmgType],
  _uimmunity :: [DmgType],
  _udmgtype :: DmgType,
  _udmg :: Int
  } deriving (Eq, Show)

data Game = Game { _forces :: [Units] }

makeClassy ''Game
type S a = StateT Game IO a

makeLenses ''Units
makePrisms ''Faction

upower :: Getter Units Int
upower = to (\x -> x^.uno * x^.udmg)

uplayOrder :: Getter Units (Int, Int)
uplayOrder = to (\x -> (x^.upower, x^.uinit))

parseDmgType :: ReadP DmgType
parseDmgType = choice
  [ (string "bludgeoning" *> pure Blunt)
  , (string "radiation" *> pure Radiation)
  , (string "cold" *> pure Cold)
  , (string "slashing" *> pure Sharp)
  , (string "fire" *> pure Fire)
  ]

parseTraitType :: String -> ReadP [DmgType]
parseTraitType t =
                   string t
                   >> string " to "
                   >> sepBy1 parseDmgType (string ", ")

parseTraits :: ReadP ([DmgType], [DmgType])
parseTraits = do
  between
    (char '(')
    (char ')')
    $ choice
          [ (,[]) <$> parseTraitType "immune"
          , (,) <$> parseTraitType "immune" <* string "; "
                <*> parseTraitType "weak"
          , (swap ... (,)) <$> parseTraitType "weak" <* string "; "
                           <*> parseTraitType "immune"
          , ([],) <$> parseTraitType "weak"
          ]

line :: Faction -> ReadP Units
line faction = do
  n <- digit
  void $ string " units each with "
  hp <- digit
  void $ string " hit points "
  (immune, weak) <- parseTraits <++ pure ([], [])
  skipSpaces
  void $ string  "with an attack that does "
  dmg <- digit
  void $ char ' '
  dmgType <- parseDmgType
  void $ string " damage at initiative "
  initiative <- digit
  eof
  pure $ Units 0 faction n hp initiative weak immune dmgType dmg

splitOnEmpty :: [String] -> ([String], [String])
splitOnEmpty xs =
  case elemIndex "" xs of
    Just i -> (take i xs, drop (i+1) xs)
    Nothing -> (xs, [])

dayO :: IO ()
dayO = do
  i <- lines <$> readFile "data/dayO.txt"
  let (immune, infection) = splitOnEmpty i
  let Just units =
        mappend <$> traverse (parse (line Good)) (drop 1 immune)
                <*> traverse (parse (line Bad)) (drop 1 infection)

  print units
  let ided_units = zipWith (\num unit -> unit & uid .~ num) [1..] units
  traverse print ided_units
  res <- evalStateT play (Game ided_units)
  printf "Part1: %d\n" (either id id res)
  pure ()


sortOn :: Ord a => (b -> a) -> [b] -> [b]
sortOn key = sortBy (compare `on` key)
maximumOn :: Ord a => (b -> a) -> [b] -> b
maximumOn key = maximumBy (compare `on` key)

traceM :: (Show a, MonadIO m) => a -> m ()
traceM = liftIO . print

resistance ::  Units -> Units -> Int
resistance a t | a^.udmgtype `elem` t^.uweakness = 2
resistance a t | a^.udmgtype `elem` t^.uimmunity = 0
resistance _ _ = 1

dmg :: Units -> Units -> Int
dmg a t = a^.upower * resistance a t

weakest :: Units -> [Units] -> Units
weakest attacker targets = snd $ maximumOn fst (f <$> targets)
 where f t = ((dmg attacker t, t^.upower, t^.uinit), t)

acquire :: [Units] -> [Units] -> [(Int, Int)]
acquire _ [] = []
acquire [] _ = []
acquire (a:as) targets = let
  enemies = filter (((/=) `on` (^.ufaction)) a) targets
  target = weakest a enemies
  in if null enemies || dmg a target == 0
    then acquire as targets
    else (a^.uid, target^.uid): acquire as (target `delete` targets)

hasId, hasntId :: Int -> [Units] -> Bool
hasId id = any (\x -> x^.uid == id)
hasntId = not ... hasId

attack :: [(Int, Int)] -> [Units] -> [Units]
attack [] units = units
attack ((a, _): rest) units | a `hasntId` units = attack rest units
attack ((_, t): rest) units | t `hasntId` units = attack rest units
attack ((a, t): rest) units = let
  units' = filter (\x -> x^.uid /= t) units
  in attack rest (if newno > 0 then (ut':units') else units')
  where get' id = fromJust $ find (\x -> x^.uid == id) units
        ua = get' a
        ut = get' t
        damage = dmg ua ut
        deaths = traceShow (show a <> " hitting " <> show t <> " for " <> show damage <> " damage, killing " <> show (damage `div` ut^.uhp))
                            damage `div` ut^.uhp
        (newno, ut') = ut & uno <-~ deaths

play :: S (Either Int Int)
play = do
  units <- use forces
  traceM $ replicate 50 '*'
  traverse traceM (filter (has (ufaction._Good)) units)
  traverse traceM (filter (has (ufaction._Bad)) units)

  -- acquire
  let target_order = reverse . sortOn (^. uplayOrder) $ units
  traceM $ replicate 50 '-'
  traverse traceM target_order
  let attacks = acquire target_order units
  if null attacks
    then let score = sum . map (^.uno) $ units
         in pure $ if any (has (ufaction._Good)) units
            then Left score
            else Right score

    else  do
      let byId x = fromJust $ find (\y -> y^.uid == fst x) units
      let attack_order = reverse . sortOn ((^.uinit) . byId) $ attacks
      forces .= attack attack_order units
      play
