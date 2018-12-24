{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TupleSections         #-}
{-# OPTIONS_GHC -Wall              #-}

module DayO where

import Control.Monad (void)
import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Data.Tuple (swap)
import Debug.Trace

import Text.ParserCombinators.ReadP
import Lib

data DmgType = Blunt | Sharp | Cold | Fire | Radiation deriving Show
type Weakness = [DmgType]
type Immunity = [DmgType]
data Units = Units {
  _uno :: Int,
  _uhp :: Int,
  _uinit :: Int,
  _uweakness :: [DmgType],
  _uimmunity :: [DmgType],
  _udmgtype :: DmgType,
  _udmg :: Int
  } deriving (Show)

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

line :: ReadP Units
line = do
  look >>= (traceShowM . take 50)
  n <- digit
  void $ string " units each with "
  hp <- digit
  void $ string " hit points "
  (immune, weak) <- parseTraits <++ pure ([], [])
  skipSpaces
  look >>= (traceShowM . take 50)
  void $ string  "with an attack that does "
  dmg <- digit
  void $ char ' '
  dmgType <- parseDmgType
  void $ string " damage at initiative "
  initiative <- digit
  eof
  pure $ Units n hp initiative weak immune  dmgType dmg

splitOnEmpty :: [String] -> ([String], [String])
splitOnEmpty xs =
  case elemIndex "" xs of
    Just i -> (take i xs, drop (i+1) xs)
    Nothing -> (xs, [])

dayO :: IO ()
dayO = do
  i <- lines <$> readFile "data/dayO.txt"
  let (immune, infection) = splitOnEmpty i
  let Just (i1, i2) =
        (,) <$> traverse (parse line) (drop 1 immune)
            <*> traverse (parse line) (drop 1 infection)

  print $ i1
  print $ i2

