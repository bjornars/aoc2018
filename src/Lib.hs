{-# LANGUAGE LambdaCase #-}

module Lib where

import Text.ParserCombinators.ReadP

(>:>) = flip $ (.) . (.)

parse :: ReadP a -> String -> Maybe a
parse  = readP_to_S >:> \case
  [(res, "")] -> Just res
  _ -> Nothing
