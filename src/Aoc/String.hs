-- |
-- Module      :  Aoc.String
-- Description :  Functions on strings
-- Copyright   :  2023 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Sun Sep 24 21:57:03 2023.
module Aoc.String
  ( stringToDigits,
    chopAt,
  )
where

import Aoc.Char

stringToDigits :: String -> [Int]
stringToDigits = map readDigit . show

chopAt :: Char -> String -> [String]
chopAt _ [] = []
chopAt c xs = case break (== c) xs of
  (w, []) -> [w]
  (w, _ : ys) -> w : chopAt c ys
