module Day5 where

import Control.Monad
import Data.Char

reactive :: Char -> Char -> Bool
reactive x y | isUpper x == isUpper y = False
reactive x y | isUpper x = reactive y x
reactive x y = y == toUpper x && not (isUpper x)

react :: String -> (String, Bool)
react  str = go str [] False
  where
    go (x : y : xs) acc _ | x `reactive` y = go xs acc True
    go (x : xs) acc changed = go xs (x : acc) changed
    go [] acc changed  = (reverse acc, changed)

recWhile f xs = let (xs', repeat) = f xs
  in if repeat
  then recWhile f xs'
  else xs'

day5 :: IO ()
day5 = do
  unless (reactive 'a' 'A' && reactive 'A' 'a') $ putStrLn "ERROR"
  when (reactive 'a' 'a' || reactive 'A' 'A' || reactive 'a' 'b') $ putStrLn "ERROR"
  input <- filter isLetter <$> readFile "data/day5.txt"
  putStrLn ("Part1: " <> show (length $ recWhile react input))
  let alternatives = (\x -> filter ((/= x).toLower) input) <$> ['a'..'z']
  let best = minimum (length . recWhile react <$> alternatives)
  putStrLn ("Part2: " <> show best)
