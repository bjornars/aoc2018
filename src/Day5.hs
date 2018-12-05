module Day5 where

import Control.Monad
import Data.Char

reactive :: Char -> Char -> Bool
reactive x y | isUpper x == isUpper y = False
reactive x y | isUpper x = reactive y x
reactive x y = y == toUpper x && not (isUpper x)

react :: String -> String
react  str = go str []
  where
    go (x : y : xs) (a : acc)  | x `reactive` y = go (a:xs) acc
    go (x : y : xs) []  | x `reactive` y = go xs []
    go (x : xs) acc = go xs (x : acc)
    go [] acc  = acc

day5 :: IO ()
day5 = do
  unless (reactive 'a' 'A' && reactive 'A' 'a') $ putStrLn "ERROR"
  when (reactive 'a' 'a' || reactive 'A' 'A' || reactive 'a' 'b') $ putStrLn "ERROR"

  input <- filter isLetter <$> readFile "data/day5.txt"
  putStrLn ("Part1: " <> (show . length . react $ input))

  let remove xs x = filter ((/= x).toLower) xs
  let best = minimum $ length . react . remove input <$> ['a'..'z']
  putStrLn ("Part2: " <> show best)
